#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <time.h>

#include <enet/enet.h>
#include <enet/types.h>

#define MS_VERSION "0.2"
#define MS_MAXSRVS 128
#define MS_MAXHOST 5
#define MS_MAXBANS 256
#define MS_TIMEOUT 100
#define MS_BANTIME (3 * 86400)
#define MS_MAXHEUR 100

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
#define LC_MS_MOTD "\nMOTD: %s\n"
#define LC_MS_URGENT "\nURGENT: %s\n"
#define LC_MS_BANNED "\nBanned %s until %s, reason: %s (#%d)\n"
#define LC_MS_NOBANS "\nCould not load ban list from file\n"
#define LC_MS_BADADR "\nBad address in file: %s\n"
#define LC_MS_BANHEUR "tripped heuristic check"
#define LC_MS_BANTOOMUCH "created too many servers"
#define LC_MS_BANSPAM "suspicious multiple server activity"
#define LC_MS_BANLIST "address in ban list"
#define LC_MS_BANTRASH "garbage server data"
#define LC_MS_BANINVAL "invalid message ID"
#define LC_MS_OOM "\nOut of memory\n"

#define MS_URGENT_FILE "urgent.txt"
#define MS_MOTD_FILE "motd.txt"
#define MS_BAN_FILE "master_bans.txt"

struct ms_ban_s;

typedef struct ms_ban_record_s {
  ENetAddress mask;
  char ip[18];
  int ban_count;
  time_t cur_ban;
  struct ms_ban_record_s *next;
  struct ms_ban_record_s *prev;
} ms_ban_record;

struct ms_server_s {
  enet_uint8 used;
  char s_ip[18];
  char s_name[257];
  char s_map[257];
  enet_uint8  s_pw;
  enet_uint8  s_plrs;
  enet_uint8  s_maxplrs;
  enet_uint8  s_mode;
  enet_uint8  s_protocol;
  enet_uint16 s_port;
  time_t      deathtime;
  time_t      lasttime;
};

typedef struct ms_server_s ms_server;

typedef struct enet_buf_s {
  enet_uint8 *data;
  size_t size;
  size_t pos;
  int overflow;
} enet_buf;

const char ms_game_ver[] = "0.63";
char ms_motd[255] = "";
char ms_urgent[255] = "";

int ms_port = 25660;
int ms_timeout = 100;
int ms_checkmultiple = 0;

enet_uint8 b_send_data[NET_BUFSIZE];
enet_buf b_send = { .data = b_send_data, .size = sizeof(b_send_data) };
enet_buf b_recv;

ENetHost  *ms_host = NULL;
ENetPeer  *ms_peers[NET_MAXCLIENTS];

ms_server  ms_srv[MS_MAXSRVS];
enet_uint8 ms_count = 0;

ms_ban_record *ms_bans;

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

void i_usage (void) {
  printf("Usage: d2df_master -p port_number [-t timeout_seconds]\n");
  fflush(stdout);
}


void i_version (void) {
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
        ms_timeout = atoi(argv[++i]);
    } else if (!strcmp(argv[i], "--check-multihost")) {
        ms_checkmultiple = 1;
    }

  }
}


int d_readtextfile (const char *fname, char *buf, size_t max) {
  FILE *f = fopen(fname, "r");
  char *const end = buf + max - 1;
  char *p = buf;
  if (f) {
    char ln[max];
    char *const lend = ln + max - 1;
    while (p < end && fgets(ln, max, f)) {
      for (char *n = ln; n < lend && *n && *n != '\r' && *n != '\n'; ++n) {
        *(p++) = *n;
        if (p == end) break;
      }
    }
    *p = '\0';
    fclose(f);
    return 0;
  }
  return 1;
}


int d_strisprint (const char *str) {
  if (!str || !*str) return 0;
  for (const char *p = str; p && *p; ++p)
    if (isprint(*p) || *p > 0x7f) return 1;
  return 0;
}


const char *d_strtime(const time_t t) {
  static char buf[128];
  struct tm *ptm = localtime(&t);
  strftime(buf, sizeof(buf), "%c", ptm);  
  return buf;
}


static inline int b_enough_left(enet_buf *buf, size_t size) {
  if (buf->pos + size > buf->size) {
    buf->overflow = 1;
    return 0;
  }
  return 1;
}


enet_uint8 b_read_uint8 (enet_buf *buf) {
  if (b_enough_left(buf, 1))
    return buf->data[buf->pos++];
  return 0;
}


enet_uint16 b_read_uint16 (enet_buf *buf) {
  enet_uint16 ret = 0;

  if (b_enough_left(buf, sizeof(ret))) {
    ret = *(enet_uint16*)(buf->data + buf->pos);
    buf->pos += sizeof(ret);
  }

  return ret;
}


char* b_read_dstring (enet_buf *buf) {
  char *ret = NULL;

  if (b_enough_left(buf, 1)) {
    size_t len = b_read_uint8(buf);
    if (b_enough_left(buf, len)) {
      ret = malloc(len + 1);
      memmove(ret, (char*)(buf->data + buf->pos), len);
      buf->pos += len;
      ret[len] = '\0';
    }
  }

  return ret;
}


void b_write_uint8 (enet_buf *buf, enet_uint8 val) {
  buf->data[buf->pos++] = val;
}


void b_write_uint16 (enet_buf *buf, enet_uint16 val) {
  *(enet_uint16*)(buf->data + buf->pos) = val;
  buf->pos += sizeof(val);
}


void b_write_dstring (enet_buf *buf, const char* val) {
  enet_uint8 len = strlen(val);
  b_write_uint8(buf, len);

  memmove((char*)(buf->data + buf->pos), val, len);
  buf->pos += len;
}


void b_write_server (enet_buf *b_send, ms_server s) {
  b_write_dstring(b_send, s.s_ip);
  b_write_uint16 (b_send, s.s_port);
  b_write_dstring(b_send, s.s_name);
  b_write_dstring(b_send, s.s_map);
  b_write_uint8  (b_send, s.s_mode);
  b_write_uint8  (b_send, s.s_plrs);
  b_write_uint8  (b_send, s.s_maxplrs);
  b_write_uint8  (b_send, s.s_protocol);
  b_write_uint8  (b_send, s.s_pw);
}


time_t ban_get_time(const int cnt) {
  static const time_t times[] = {
       1 *  5 * 60,
       1 * 30 * 60,
       1 * 60 * 60,
      24 * 60 * 60,
      72 * 60 * 60,
    8760 * 60 * 60,
  };

  static const size_t numtimes = sizeof(times) / sizeof(*times);

  if (cnt >= numtimes || cnt < 0)
    return times[numtimes - 1];

  return times[cnt];
}


ms_ban_record *ban_check (const ENetAddress *addr) {
  const time_t now = time(NULL);

  for (ms_ban_record *b = ms_bans; b; b = b->next) {
    if (b->mask.host == addr->host) {
      if (b->cur_ban > now)
        return b;
    }
  }

  return NULL;
}


ms_ban_record *ban_record_check (const ENetAddress *addr) {
  for (ms_ban_record *b = ms_bans; b; b = b->next) {
    if (b->mask.host == addr->host)
      return b;
  }
  return NULL;
}


ms_ban_record *ban_record_add_addr (const ENetAddress *addr, const int cnt, const time_t cur) {
  ms_ban_record *rec = ban_record_check(addr);
  if (rec) return rec;

  rec = calloc(1, sizeof(*rec));
  if (!rec) return NULL;

  enet_address_get_host_ip(addr, rec->ip, 17);
  rec->mask = *addr;
  rec->ban_count = cnt;
  rec->cur_ban = cur;

  if (ms_bans) ms_bans->prev = rec;
  rec->next = ms_bans;
  ms_bans = rec;

  return rec;
}


ms_ban_record *ban_record_add_ip (const char *ip, const int cnt, const time_t cur) {
  ENetAddress addr;
  if (enet_address_set_host_ip(&addr, ip) != 0) {
    fprintf(stderr, LC_MS_BADADR, ip);
    return NULL;
  }
  return ban_record_add_addr(&addr, cnt, cur);
}


void ban_load_list (const char *fname) {
  FILE *f = fopen(fname, "r");
  if (!f) {
      d_error(LC_MS_NOBANS, 0);
      return;
  }

  char ln[256] = { 0 };

  while (fgets(ln, sizeof(ln), f)) {
    for (int i = sizeof(ln) - 1; i >= 0; --i)
      if (ln[i] == '\n' || ln[i] == '\r')
        ln[i] = 0;

    char ip[17] = { 0 };
    time_t exp = 0;
    int count = 0;

    sscanf(ln, "%16s %ld %d", ip, &exp, &count);

    if (ban_record_add_ip(ip, count, exp))
      printf(LC_MS_BANNED, ip, d_strtime(exp), LC_MS_BANLIST, count);
  }

  fclose(f);
}


void ban_save_list (const char *fname) {
  FILE *f = fopen(fname, "w");
  if (!f) {
    d_error(LC_MS_NOBANS, 0);
    return;
  }

  for (ms_ban_record *rec = ms_bans; rec; rec = rec->next)
    if (rec->ban_count)
      fprintf(f, "%s %ld %d\n", rec->ip, rec->cur_ban, rec->ban_count);

  fclose(f);
}


int ban_heur (const ms_server *srv, const time_t now) {
  int score = 0;

  // can't have more than 24 maxplayers; can't have more than max
  if (srv->s_plrs > srv->s_maxplrs || srv->s_maxplrs > 24 || srv->s_maxplrs == 0)
    score += MS_MAXHEUR;

  // name and map have to be non-garbage
  if (!d_strisprint(srv->s_map) || !d_strisprint(srv->s_name))
    score += MS_MAXHEUR;

  // these protocols don't exist
  if (srv->s_protocol < 100 || srv->s_protocol > 250)
    score += MS_MAXHEUR;

  // the game doesn't allow server names longer than 64 chars
  if (strlen(srv->s_name) > 64)
    score += MS_MAXHEUR;

  // game mode has to actually exist
  if (srv->s_mode > 5)
    score += MS_MAXHEUR;

  // password field can be either 0 or 1
  if (srv->s_pw > 1)
    score += MS_MAXHEUR;

  // port has to be set, although the game allows you to set it to 0
  // if (!srv->s_port)
  //   score += MS_MAXHEUR;

  // servers usually don't update more often than once every 30 seconds
  if (now - srv->lasttime < 5)
    score += MS_MAXHEUR / 2;

  return score;
}

void erase_banned_host(const char *ip) {
  for (int i = 0; i < MS_MAXSRVS; ++i) {
    if (!strcmp(ms_srv[i].s_ip, ip)) {
      if (ms_srv[i].used) {
        ms_srv[i].used = 0;
        ms_count--;
      }
    }
  }
}

time_t get_sum_lasttime(char* ip) {
  time_t sumLastTime = 0;
  const time_t now = time(NULL);
  for (int i = 0; i < MS_MAXSRVS; ++i) {
    if (ms_srv[i].used && (strncmp(ip, ms_srv[i].s_ip, 16) == 0)) {
      sumLastTime = sumLastTime + (now - ms_srv[i].lasttime);
    }
  }
  return sumLastTime;
}

void ban_add (const ENetAddress *addr, const char *reason) {
  const time_t now = time(NULL);

  ms_ban_record *rec = ban_record_add_addr(addr, 0, 0);
  if (!rec) d_error(LC_MS_OOM, 1);

  rec->cur_ban = now + ban_get_time(rec->ban_count);
  rec->ban_count++;

  printf(LC_MS_BANNED, rec->ip, d_strtime(rec->cur_ban), reason, rec->ban_count);

  ban_save_list(MS_BAN_FILE);
  erase_banned_host(rec->ip);
}


void d_deinit(void) {
  ban_save_list(MS_BAN_FILE);
}

int count_servers(char* ip) {
  int sameHostServers = 0;
  for (int i = 0; i < MS_MAXSRVS; ++i) {
    if ((strncmp(ip, ms_srv[i].s_ip, 16) == 0)) {
      ++sameHostServers;
    }
  }
  return sameHostServers;
}


#define CHECK_RECV_OVERFLOW(addr) \
  if (b_recv.overflow) { \
    ban_add(addr, LC_MS_BANTRASH); \
    break; \
  }

int main (int argc, char *argv[]) {
  d_getargs(argc, argv);

  if (enet_initialize()) {
    d_error("Could not init ENet!", 1);
    return EXIT_FAILURE;
  }

  printf(LC_MS_INIT, ms_port);

  d_readtextfile(MS_MOTD_FILE, ms_motd, sizeof(ms_motd));
  d_readtextfile(MS_URGENT_FILE, ms_urgent, sizeof(ms_urgent));
  ban_load_list(MS_BAN_FILE);

  if (ms_motd[0]) printf(LC_MS_MOTD, ms_motd);
  if (ms_urgent[0]) printf(LC_MS_URGENT, ms_urgent);

  for (int i = 0; i < NET_MAXCLIENTS; ++i) ms_peers[i] = NULL;

  for (int i = 0; i < MS_MAXSRVS; ++i) {
    ms_srv[i].used = 0;
    ms_srv[i].s_ip[0] = '\0';
    ms_srv[i].s_name[0] = '\0';
    ms_srv[i].s_map[0] = '\0';
    ms_srv[i].deathtime = 0;
  }

  ENetAddress addr;
  addr.host = ENET_HOST_ANY;
  addr.port = ms_port;

  ms_host = enet_host_create(&addr, NET_MAXCLIENTS, NET_CHANS, 0, 0);
  if (!ms_host) {
    d_error("Could not create host on specified port!", 1);
    return EXIT_FAILURE;
  }

  atexit(d_deinit);

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
    while (enet_host_service(ms_host, &event, 5000) > 0) {
      if (event.peer && ban_check(&(event.peer->address)))
        continue;

      const time_t now = time(NULL);

      switch (event.type) {
        case ENET_EVENT_TYPE_CONNECT:
          printf(LC_MS_CONN, event.peer->address.host, event.peer->address.port);
          break;

        case ENET_EVENT_TYPE_RECEIVE:
          if (!event.peer) continue;

          b_recv.pos = 0;
          b_recv.overflow = 0;
          b_recv.data = event.packet->data;
          b_recv.size = event.packet->dataLength;
          msg = b_read_uint8(&b_recv);

          switch (msg) {
            case NET_MSG_ADD:
              enet_address_get_host_ip(&(event.peer->address), ip, 17);
              port = b_read_uint16(&b_recv);

              name = b_read_dstring(&b_recv);
              map = b_read_dstring(&b_recv);
              gm  = b_read_uint8(&b_recv);

              pl = b_read_uint8(&b_recv);
              mpl = b_read_uint8(&b_recv);

              proto = b_read_uint8(&b_recv);
              pw = b_read_uint8(&b_recv);

              CHECK_RECV_OVERFLOW(&(event.peer->address));

              for (int i = 0; i < MS_MAXSRVS; ++i) {
                if (ms_srv[i].used) {
                  if ((strncmp(ip, ms_srv[i].s_ip, 16) == 0) && (ms_srv[i].s_port == port)) {
                    strncpy(ms_srv[i].s_map, map, sizeof(ms_srv[i].s_map) - 1);
                    strncpy(ms_srv[i].s_name, name, sizeof(ms_srv[i].s_name) - 1);
                    ms_srv[i].s_plrs = pl;
                    ms_srv[i].s_maxplrs = mpl;
                    ms_srv[i].s_pw = pw;
                    ms_srv[i].s_mode = gm;

                    if (ban_heur(ms_srv + i, now) >= MS_MAXHEUR) {
                      ban_add(&(event.peer->address), LC_MS_BANHEUR);
                      break;
                    }

                    ms_srv[i].deathtime = now + ms_timeout;
                    ms_srv[i].lasttime = now;

                    printf(LC_MS_UPD, i, ip, port, name, map, gm, pl, mpl, proto, pw);
                    break;
                  }
                } else {
                    int countServer = count_servers(ip);
                    if (countServer > MS_MAXHOST) {
                      ban_add(&(event.peer->address), LC_MS_BANTOOMUCH);
                      break;
                    }
                    else if (ms_checkmultiple && countServer > 1) {
                      if (get_sum_lasttime(ip) < (countServer*3)) {
                        ban_add(&(event.peer->address), LC_MS_BANSPAM);
                        break;
                      }
                    }
                    strncpy(ms_srv[i].s_ip, ip, sizeof(ms_srv[i].s_ip) - 1);
                    strncpy(ms_srv[i].s_map, map, sizeof(ms_srv[i].s_map) - 1);
                    strncpy(ms_srv[i].s_name, name, sizeof(ms_srv[i].s_name) - 1);
                    ms_srv[i].s_port = port;
                    ms_srv[i].s_plrs = pl;
                    ms_srv[i].s_maxplrs = mpl;
                    ms_srv[i].s_pw = pw;
                    ms_srv[i].s_mode = gm;
                    ms_srv[i].s_protocol = proto;
                    ms_srv[i].deathtime = now + ms_timeout;
                    ms_srv[i].lasttime = now;

                    if (ban_heur(ms_srv + i, now) >= MS_MAXHEUR) {
                      ban_add(&(event.peer->address), LC_MS_BANHEUR);
                      break;
                    }

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
              port = b_read_uint16(&b_recv);
              CHECK_RECV_OVERFLOW(&(event.peer->address));
              for (int i = 0; i < MS_MAXSRVS; ++i) {
                if (ms_srv[i].used) {
                  if ((strncmp(ip, ms_srv[i].s_ip, 16) == 0) && (ms_srv[i].s_port == port)) {
                    if (ban_heur(ms_srv + i, now) >= MS_MAXHEUR) {
                      ban_add(&(event.peer->address), LC_MS_BANHEUR);
                      break;
                    }
                    ms_srv[i].used = 0;
                    printf(LC_MS_RM, i, ip, port);
                    --ms_count;
                  }
                }
              }
              break;

            case NET_MSG_LIST:
              b_send.pos = 0;
              b_write_uint8(&b_send, NET_MSG_LIST);

              if (event.packet->dataLength > 2) {
                // holy shit a fresh client
                clientver = b_read_dstring(&b_recv);
                b_write_uint8(&b_send, ms_count);
              } else {
                // old client, feed them bullshit first
                b_write_uint8(&b_send, ms_count + 2);
                for (int i = 0; i < MS_FAKESRVS; ++i)
                  b_write_server(&b_send, ms_fake_srv[i]);
              }

              CHECK_RECV_OVERFLOW(&(event.peer->address));

              for (int i = 0; i < MS_MAXSRVS; ++i) {
                if (ms_srv[i].used)
                  b_write_server(&b_send, ms_srv[i]);
              }

              if (clientver) {
                // TODO: check if this client is outdated (?) and send back new verstring
                // for now just write the same shit back
                b_write_dstring(&b_send, clientver);
                // write the motd and urgent message
                b_write_dstring(&b_send, ms_motd);
                b_write_dstring(&b_send, ms_urgent);
              }

              ENetPacket *p = enet_packet_create(b_send.data, b_send.pos, ENET_PACKET_FLAG_RELIABLE);
              enet_peer_send(event.peer, NET_CH_MAIN, p);
              enet_host_flush(ms_host);

              printf(LC_MS_LIST, event.peer->address.host, event.peer->address.port, clientver ? clientver : "<old>");
              free(clientver);
              clientver = NULL;
              break;

            default:
              // cheeky cunt sending invalid messages
              ban_add(&(event.peer->address), LC_MS_BANINVAL);
              break;
          }

          enet_packet_destroy(event.packet);
          break;

        default:
          break;
      }
    }

    time_t now = time(NULL);
    for (int i = 0; i < MS_MAXSRVS; ++i) {
      if (ms_srv[i].used) {
        if (ms_srv[i].deathtime <= now) {
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
