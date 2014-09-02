unit ENet_Protocol;

{
  ENet - Reliable UDP networking library
  Delphi 7 DLL header by Chernov D. Dmitry aka Black Doomer
  Original file: protocol.h
  16.08.2014
}

interface

uses ENet_Types;

const
   ENET_PROTOCOL_MINIMUM_MTU             = 576;
   ENET_PROTOCOL_MAXIMUM_MTU             = 4096;
   ENET_PROTOCOL_MAXIMUM_PACKET_COMMANDS = 32;
   ENET_PROTOCOL_MINIMUM_WINDOW_SIZE     = 4096;
   ENET_PROTOCOL_MAXIMUM_WINDOW_SIZE     = 65536;
   ENET_PROTOCOL_MINIMUM_CHANNEL_COUNT   = 1;
   ENET_PROTOCOL_MAXIMUM_CHANNEL_COUNT   = 255;
   ENET_PROTOCOL_MAXIMUM_PEER_ID         = $FFF;
   ENET_PROTOCOL_MAXIMUM_FRAGMENT_COUNT  = 1024 * 1024;

type
  ENetProtocolCommand = ( ENET_PROTOCOL_COMMAND_NONE,
                          ENET_PROTOCOL_COMMAND_ACKNOWLEDGE,
                          ENET_PROTOCOL_COMMAND_CONNECT,
                          ENET_PROTOCOL_COMMAND_VERIFY_CONNECT,
                          ENET_PROTOCOL_COMMAND_DISCONNECT,
                          ENET_PROTOCOL_COMMAND_PING,
                          ENET_PROTOCOL_COMMAND_SEND_RELIABLE,
                          ENET_PROTOCOL_COMMAND_SEND_UNRELIABLE,
                          ENET_PROTOCOL_COMMAND_SEND_FRAGMENT,
                          ENET_PROTOCOL_COMMAND_SEND_UNSEQUENCED,
                          ENET_PROTOCOL_COMMAND_BANDWIDTH_LIMIT,
                          ENET_PROTOCOL_COMMAND_THROTTLE_CONFIGURE,
                          ENET_PROTOCOL_COMMAND_SEND_UNRELIABLE_FRAGMENT,
                          ENET_PROTOCOL_COMMAND_COUNT,
                          ENET_PROTOCOL_COMMAND_MASK = $0F                );

  ENetProtocolFlag    = ( ENET_PROTOCOL_COMMAND_FLAG_ACKNOWLEDGE = 1 shl 7,
                          ENET_PROTOCOL_COMMAND_FLAG_UNSEQUENCED = 1 shl 6,
                          ENET_PROTOCOL_HEADER_FLAG_COMPRESSED   = 1 shl 14,
                          ENET_PROTOCOL_HEADER_FLAG_SENT_TIME    = 1 shl 15,
                          ENET_PROTOCOL_HEADER_FLAG_MASK         = ENET_PROTOCOL_HEADER_FLAG_COMPRESSED or Word( ENET_PROTOCOL_HEADER_FLAG_SENT_TIME ),
                          ENET_PROTOCOL_HEADER_SESSION_MASK      = 3 shl 12,
                          ENET_PROTOCOL_HEADER_SESSION_SHIFT     = 12        );

  pENetProtocolHeader = ^ENetProtocolHeader;
  ENetProtocolHeader = packed record
    peerID   : enet_uint16;
    sentTime : enet_uint16;
  end;

  pENetProtocolCommandHeader = ^ENetProtocolCommandHeader;
  ENetProtocolCommandHeader = packed record
    command                : enet_uint8;
    channelID              : enet_uint8;
    reliableSequenceNumber : enet_uint16;
  end;

  pENetProtocolAcknowledge = ^ENetProtocolAcknowledge;
  ENetProtocolAcknowledge = packed record
    header                         : ENetProtocolCommandHeader;
    receivedReliableSequenceNumber : enet_uint16;
    receivedSentTime               : enet_uint16;
  end;

  pENetProtocolConnect = ^ENetProtocolConnect;
  ENetProtocolConnect = packed record
    header                     : ENetProtocolCommandHeader;
    outgoingPeerID             : enet_uint16;
    incomingSessionID          : enet_uint8;
    outgoingSessionID          : enet_uint8;
    mtu                        : enet_uint32;
    windowSize                 : enet_uint32;
    channelCount               : enet_uint32;
    incomingBandwidth          : enet_uint32;
    outgoingBandwidth          : enet_uint32;
    packetThrottleInterval     : enet_uint32;
    packetThrottleAcceleration : enet_uint32;
    packetThrottleDeceleration : enet_uint32;
    connectID                  : enet_uint32;
    data                       : enet_uint32;
  end;

  pENetProtocolVerifyConnect = ^ENetProtocolVerifyConnect;
  ENetProtocolVerifyConnect = packed record
    header                     : ENetProtocolCommandHeader;
    outgoingPeerID             : enet_uint16;
    incomingSessionID          : enet_uint8;
    outgoingSessionID          : enet_uint8;
    mtu                        : enet_uint32;
    windowSize                 : enet_uint32;
    channelCount               : enet_uint32;
    incomingBandwidth          : enet_uint32;
    outgoingBandwidth          : enet_uint32;
    packetThrottleInterval     : enet_uint32;
    packetThrottleAcceleration : enet_uint32;
    packetThrottleDeceleration : enet_uint32;
    connectID                  : enet_uint32;
  end;

  pENetProtocolBandwidthLimit = ^ENetProtocolBandwidthLimit;
  ENetProtocolBandwidthLimit = packed record
    header            : ENetProtocolCommandHeader;
    incomingBandwidth : enet_uint32;
    outgoingBandwidth : enet_uint32;
  end;

  pENetProtocolThrottleConfigure = ^ENetProtocolThrottleConfigure;
  ENetProtocolThrottleConfigure = packed record
    header                     : ENetProtocolCommandHeader;
    packetThrottleInterval     : enet_uint32;
    packetThrottleAcceleration : enet_uint32;
    packetThrottleDeceleration : enet_uint32;
  end;

  pENetProtocolDisconnect = ^ENetProtocolDisconnect;
  ENetProtocolDisconnect = packed record
    header : ENetProtocolCommandHeader;
    data   : enet_uint32;
  end;

  pENetProtocolPing = ^ENetProtocolPing;
  ENetProtocolPing = packed record
    header : ENetProtocolCommandHeader;
  end;

  pENetProtocolSendReliable = ^ENetProtocolSendReliable;
  ENetProtocolSendReliable = packed record
    header     : ENetProtocolCommandHeader;
    dataLength : enet_uint16;
  end;

  pENetProtocolSendUnreliable = ^ENetProtocolSendUnreliable;
  ENetProtocolSendUnreliable = packed record
    header                   : ENetProtocolCommandHeader;
    unreliableSequenceNumber : enet_uint16;
    dataLength               : enet_uint16;
  end;

  pENetProtocolSendUnsequenced = ^ENetProtocolSendUnsequenced;
  ENetProtocolSendUnsequenced = packed record
    header           : ENetProtocolCommandHeader;
    unsequencedGroup : enet_uint16;
    dataLength       : enet_uint16;
  end;

  pENetProtocolSendFragment = ^ENetProtocolSendFragment;
  ENetProtocolSendFragment = packed record
    header              : ENetProtocolCommandHeader;
    startSequenceNumber : enet_uint16;
    dataLength          : enet_uint16;
    fragmentCount       : enet_uint32;
    fragmentNumber      : enet_uint32;
    totalLength         : enet_uint32;
    fragmentOffset      : enet_uint32;
  end;

  pENetProtocol = ^ENetProtocol;
  ENetProtocol = packed record //union
  case Byte of
  0 : (header            : ENetProtocolCommandHeader);
  1 : (acknowledge       : ENetProtocolAcknowledge);
  2 : (connect           : ENetProtocolConnect);
  3 : (verifyConnect     : ENetProtocolVerifyConnect);
  4 : (disconnect        : ENetProtocolDisconnect);
  5 : (ping              : ENetProtocolPing);
  6 : (sendReliable      : ENetProtocolSendReliable);
  7 : (sendUnreliable    : ENetProtocolSendUnreliable);
  8 : (sendUnsequenced   : ENetProtocolSendUnsequenced);
  9 : (sendFragment      : ENetProtocolSendFragment);
  10: (bandwidthLimit    : ENetProtocolBandwidthLimit);
  11: (throttleConfigure : ENetProtocolThrottleConfigure);
  end;

implementation

end.
