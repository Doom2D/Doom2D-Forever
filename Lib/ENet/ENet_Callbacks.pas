unit ENet_Callbacks;

{
  ENet - Reliable UDP networking library
  Delphi 7 DLL header by Chernov D. Dmitry aka Black Doomer
  Original file: callbacks.h
  16.08.2014
}

interface

uses ENet_Types; //only for size_t

type
  pENetCallbacks = ^ENetCallbacks;
  ENetCallbacks = record
    malloc    : function( size: enet_size_t ): Pointer; cdecl;
    free      : procedure( memory: Pointer ); cdecl;
    no_memory : procedure(); cdecl;
  end;

implementation

end.
