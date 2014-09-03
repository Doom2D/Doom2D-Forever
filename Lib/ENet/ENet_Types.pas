unit ENet_Types;

{
  ENet - Reliable UDP networking library
  Delphi 7 DLL header by Chernov D. Dmitry aka Black Doomer
  Original file: types.h
  16.08.2014
}

interface

type
  enet_size_t = Cardinal; //alias for C size_t
  penet_size_t = ^enet_size_t;

  enet_int = SmallInt; //alias for C int
  penet_int = ^enet_int;

  enet_uint8 = Byte;
  penet_uint8 = ^enet_uint8;

  enet_uint16 = Word;
  penet_uint16 = ^enet_uint16;

  enet_uint32 = LongWord;
  penet_uint32 = ^enet_uint32;

implementation

end.
