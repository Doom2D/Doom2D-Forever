#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <enet/enet.h>
#include <enet/types.h>

#define MS_VERSION "0.2"
#define MS_MAXSRVS 128
#define MS_TIMEOUT 100

#define NET_CHANS 2
#define NET_CH_MAIN 0
#define NET_CH_UPD  1
#define NET_MAXCLIENTS 64

#define NET_BUFSIZE 65536

#define NET_MSG_ADD  200
#define NET_MSG_RM   201
#define NET_MSG_LIST 202

#define LC_MS_INIT "D2DF master server starting on port %d...\n"
#define LC_MS_ADD  "\nAdded server in slot #%d:\n%s:%d\n%s\n%s (%d)\n%d/%d plrs\nproto: %d pw?: %d\n"
#define LC_MS_UPD  "\nUpdated server #%d (%s:%d):\n%s\n%s (%d)\n%d/%d plrs\nproto: %d pw?: %d\n"
#define LC_MS_RM   "\nRemoved server #%d (%s:%d) by request.\n"
#define LC_MS_TIME "\nServer #%d (%s:%d) timed out.\n"
#define LC_MS_LIST "\nSent server list to %x:%u (ver. %s).\n"
#define LC_MS_DIE  "\nD2DF master server shutting down...\n"
#define LC_MS_CONN "\nIncoming connection from %x:%u...\n"


struct ms_server_s {
  enet_uint8 used;
  char s_ip[17];
  char s_name[256];
  char s_map[256];
  enet_uint8  s_pw;
  enet_uint8  s_plrs;
  enet_uint8  s_maxplrs;
  enet_uint8  s_mode;
  enet_uint8  s_protocol;
  enet_uint16 s_port;
  enet_uint32 ttl;
};

typedef struct ms_server_s ms_server;

const char ms_game_ver[] = "0.63";
int ms_port = 25660;
int ms_timeout = 100000;

size_t b_read = 0;
size_t b_write = 0;

enet_uint8 b_send[NET_BUFSIZE];

ENetHost  *ms_host = NULL;
ENetPeer  *ms_peers[NET_MAXCLIENTS];
ms_server  ms_srv[MS_MAXSRVS];
enet_uint8 ms_count = 0;

// fake servers to show on old versions of the game
static const ms_server ms_fake_srv[] = {
  {
    .used = 1,
    .s_ip = "0.0.0.0",
    .s_name = "! \xc2\xc0\xd8\xc0 \xca\xce\xcf\xc8\xdf \xc8\xc3\xd0\xdb "
              "\xd3\xd1\xd2\xc0\xd0\xc5\xcb\xc0! "
              "\xd1\xca\xc0\xd7\xc0\xc9\xd2\xc5 \xcd\xce\xc2\xd3\xde C "
              "doom2d.org !",
    .s_map = "! Your game is outdated. "
             "Get latest version at doom2d.org !",
    .s_protocol = 255,
  },
  {
    .used = 1,
    .s_ip = "0.0.0.0",
    .s_name = "! \xcf\xd0\xce\xc1\xd0\xce\xd1\xdcTE \xcf\xce\xd0\xd2\xdb "
              "25666 \xc8 57133 HA CEPBEPE \xcf\xc5\xd0\xc5\xc4 \xc8\xc3\xd0\xce\xc9 !",
    .s_map = "! Forward ports 25666 and 57133 before hosting !",
    .s_protocol = 255,
  },
};

#define MS_FAKESRVS (sizeof(ms_fake_srv) / sizeof(ms_fake_srv[0]))

void i_usage () {
  printf("Usage: d2df_master -p port_number [-t timeout_seconds]\n");
  fflush(stdout);
}


void i_version () {
  printf("Doom 2D Forever master server v%s\n", MS_VERSION);
  fflush(stdout);
}


void d_error (const char *msg, int fatal) {
  if (fatal) {
    fprintf(stderr, "FATAL ERROR: %s\n", msg);
    exit(EXIT_FAILURE);
  } else {
    fprintf(stderr, "ERROR: %s\n", msg);
  }
}


void d_getargs (int argc, char *argv[]) {
  if (argc < 2) {
    i_usage();
    exit(0);
    return;
  }

  for (int i = 1; i < argc; ++i) {
    if (!strcmp(argv[i], "-v")) {
      i_version();
      exit(0);
      return;
    } else if (!strcmp(argv[i], "-p")) {
      if (i + 1 >= argc) {
        d_error("Specify a port value!", 1);
        return;
      } else {
        ms_port = atoi(argv[++i]);
      }
    } else if (!strcmp(argv[i], "-t") & (i + 1 < argc)) {
        ms_timeout = atoi(argv[++i]) * 1000;
    }
  }
}


enet_uint8  b_read_uint8 (enet_uint8 buf[], size_t *pos) {
  return buf[(*pos)++];
}


enet_uint16 b_read_uint16 (enet_uint8 buf[], size_t *pos) {
  enet_uint16 ret = 0;

  ret = *(enet_uint16*)(buf + *pos);
  *pos += sizeof(enet_uint16);

  return ret;
}


char* b_read_dstring (enet_uint8 buf[], size_t *pos) {
  char *ret = NULL;

  size_t len = b_read_uint8(buf, pos);

  ret = malloc(len + 1);

  memmove(ret, (char*)(buf + *pos), len);
  ret[len] = '\0';
  *pos += len;

  return ret;
}


void b_write_uint8 (enet_uint8 buf[], size_t *pos, enet_uint8 val) {
  buf[(*pos)++] = val;
}


void b_write_uint16 (enet_uint8 buf[], size_t *pos, enet_uint16 val) {
  *(enet_uint16*)(buf + *pos) = val;
  *pos += sizeof(enet_uint16);
}


void b_write_dstring (enet_uint8 buf[], size_t *pos, const char* val) {
  enet_uint8 len = strlen(val);
  b_write_uint8(buf, pos, len);

  memmove((char*)(buf + *pos), val, len);
  *pos += len;
}


void b_write_server (enet_uint8 buf[], size_t *pos, ms_server s) {
  b_write_dstring(b_send, pos, s.s_ip);
  b_write_uint16 (b_send, pos, s.s_port);
  b_write_dstring(b_send, pos, s.s_name);
  b_write_dstring(b_send, pos, s.s_map);
  b_write_uint8  (b_send, pos, s.s_mode);
  b_write_uint8  (b_send, pos, s.s_plrs);
  b_write_uint8  (b_send, pos, s.s_maxplrs);
  b_write_uint8  (b_send, pos, s.s_protocol);
  b_write_uint8  (b_send, pos, s.s_pw);
}


int main (int argc, char *argv[]) {
  d_getargs(argc, argv);

  if (enet_initialize()) {
    d_error("Could not init ENet!", 1);
    return EXIT_FAILURE;
  }

  printf(LC_MS_INIT, ms_port);

  for (int i = 0; i < NET_MAXCLIENTS; ++i) ms_peers[i] = NULL;

  for (int i = 0; i < MS_MAXSRVS; ++i) {
    ms_srv[i].used = 0;
    ms_srv[i].s_ip[0] = '\0';
    ms_srv[i].s_name[0] = '\0';
    ms_srv[i].s_map[0] = '\0';
    ms_srv[i].ttl = 0;
  }

  ENetAddress addr;
  addr.host = ENET_HOST_ANY;
  addr.port = ms_port;

  ms_host = enet_host_create(&addr, NET_MAXCLIENTS, NET_CHANS, 0, 0);
  if (!ms_host) {
    d_error("Could not create host on specified port!", 1);
    return EXIT_FAILURE;
  }

  atexit(enet_deinitialize);

  ENetEvent event;
  int shutdown = 0;
  enet_uint8 msg = 255;

  char ip[17];
  enet_uint16 port = 0;

  char *name = NULL;
  char *map = NULL;
  char *clientver = NULL;
  enet_uint8 gm = 0;
  enet_uint16 pl = 0;
  enet_uint16 mpl = 0;

  enet_uint8 proto = 0;
  enet_uint8 pw = 0;
  while (!shutdown) {
    while (enet_host_service(ms_host, &event, 1) > 0) {
      switch (event.type) {
        case ENET_EVENT_TYPE_CONNECT:
          printf(LC_MS_CONN, event.peer->address.host, event.peer->address.port);
          break;
        case ENET_EVENT_TYPE_RECEIVE:
          if (!event.peer) continue;
          b_read = 0;
          msg = b_read_uint8(event.packet->data, &b_read);

          switch (msg) {
            case NET_MSG_ADD:
              if (!event.peer) continue;

              enet_address_get_host_ip(&(event.peer->address), ip, 17);
              port = b_read_uint16(event.packet->data, &b_read);

              name = b_read_dstring(event.packet->data, &b_read);
              map = b_read_dstring(event.packet->data, &b_read);
              gm  = b_read_uint8(event.packet->data, &b_read);

              pl = b_read_uint8(event.packet->data, &b_read);
              mpl = b_read_uint8(event.packet->data, &b_read);

              proto = b_read_uint8(event.packet->data, &b_read);
              pw = b_read_uint8(event.packet->data, &b_read);

              for (int i = 0; i < MS_MAXSRVS; ++i) {
                if (ms_srv[i].used) {
                  if ((strncmp(ip, ms_srv[i].s_ip, 16) == 0) && (ms_srv[i].s_port == port)) {
                    strncpy(ms_srv[i].s_map, map, sizeof(ms_srv[i].s_map));
                    strncpy(ms_srv[i].s_name, name, sizeof(ms_srv[i].s_name));
                    ms_srv[i].s_plrs = pl;
                    ms_srv[i].s_maxplrs = mpl;
                    ms_srv[i].s_pw = pw;
                    ms_srv[i].s_mode = gm;

                    ms_srv[i].ttl = ms_timeout;

                    printf(LC_MS_UPD, i, ip, port, name, map, gm, pl, mpl, proto, pw);
                    break;
                  }
                } else {
                    strncpy(ms_srv[i].s_ip, ip, sizeof(ms_srv[i].s_ip));
                    strncpy(ms_srv[i].s_map, map, sizeof(ms_srv[i].s_map));
                    strncpy(ms_srv[i].s_name, name, sizeof(ms_srv[i].s_name));
                    ms_srv[i].s_port = port;
                    ms_srv[i].s_plrs = pl;
                    ms_srv[i].s_maxplrs = mpl;
                    ms_srv[i].s_pw = pw;
                    ms_srv[i].s_mode = gm;
                    ms_srv[i].s_protocol = proto;
                    ms_srv[i].ttl = ms_timeout;

                    ms_srv[i].used = 1;

                    printf(LC_MS_ADD, i, ip, port, name, map, gm, pl, mpl, proto, pw);

                    ++ms_count;
                    break;
                }
              }
              free(name);
              free(map);
              break;
            case NET_MSG_RM:
              enet_address_get_host_ip(&(event.peer->address), ip, 17);
              port = b_read_uint16(event.packet->data, &b_read);
              for (int i = 0; i < MS_MAXSRVS; ++i) {
                if (ms_srv[i].used) {
                  if ((strncmp(ip, ms_srv[i].s_ip, 16) == 0) && (ms_srv[i].s_port == port)) {
                    ms_srv[i].used = 0;
                    printf(LC_MS_RM, i, ip, port);
                    --ms_count;
                  }
                }
              }
              break;
            case NET_MSG_LIST:
              if (!event.peer) continue;

              b_write = 0;
              b_write_uint8(b_send, &b_write, NET_MSG_LIST);

              if (event.packet->dataLength > 2) {
                // holy shit a fresh client
                clientver = b_read_dstring(event.packet->data, &b_read);
                b_write_uint8(b_send, &b_write, ms_count);
              } else {
                // old client, feed them bullshit first
                b_write_uint8(b_send, &b_write, ms_count + 2);
                for (int i = 0; i < MS_FAKESRVS; ++i)
                  b_write_server(b_send, &b_write, ms_fake_srv[i]);
              }

              for (int i = 0; i < MS_MAXSRVS; ++i) {
                if (ms_srv[i].used) b_write_server(b_send, &b_write, ms_srv[i]);
              }

              if (clientver) {
                // TODO: check if this client is outdated (?) and send back new verstring
                // for now just write the same shit back
                b_write_dstring(b_send, &b_write, clientver);
              }

              ENetPacket *p = enet_packet_create(b_send, b_write, ENET_PACKET_FLAG_RELIABLE);
              enet_peer_send(event.peer, NET_CH_MAIN, p);
              enet_host_flush(ms_host);

              printf(LC_MS_LIST, event.peer->address.host, event.peer->address.port, clientver ? clientver : "<old>");
              free(clientver);
              clientver = NULL;
              break;
          }

          enet_packet_destroy(event.packet);
          break;

        default:
          break;
      }
    }

    for (int i = 0; i < MS_MAXSRVS; ++i) {
      if (ms_srv[i].used) {
        if (--(ms_srv[i].ttl) == 0) {
          ms_srv[i].used = 0;
          printf(LC_MS_TIME, i, ms_srv[i].s_ip, ms_srv[i].s_port);
          --ms_count;
        }
      }
    }
  }

  printf(LC_MS_DIE);

  return EXIT_SUCCESS;
}
