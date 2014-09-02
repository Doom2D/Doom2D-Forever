unit ENet_List;

{
  ENet - Reliable UDP networking library
  Delphi 7 DLL header by Chernov D. Dmitry aka Black Doomer
  Original file: list.h
  16.08.2014
}

interface

uses ENet_Types; //only for size_t
                                  
type
  pENetListNode = ^ENetListNode;
  ENetListNode = record
    next     : pENetListNode;
    previous : pENetListNode;
  end;

  ENetListIterator = pENetListNode;

  pENetList = ^ENetList;
  ENetList = record
    sentinel : ENetListNode;
  end;

//inline macros
function enet_list_begin( list: pENetList ): ENetListIterator; // inline;
function enet_list_end( list: pENetList ): ENetListIterator; // inline;

function enet_list_empty( list: pENetList ): Boolean; // inline;

function enet_list_next( iterator: ENetListIterator ): ENetListIterator; // inline;
function enet_list_previous( iterator: ENetListIterator ): ENetListIterator; // inline;

function enet_list_front( list: pENetList ): Pointer; // inline;
function enet_list_back( list: pENetList ): Pointer; // inline;

implementation

function enet_list_begin;
   begin Result := list^.sentinel.next;
     end;
function enet_list_end;
   begin Result := @( list^.sentinel );
     end;

function enet_list_empty;
   begin Result := enet_list_begin(list) = enet_list_end(list);
     end;

function enet_list_next;
   begin Result := iterator^.next;
     end;
function enet_list_previous;
   begin Result := iterator^.previous;
     end;

function enet_list_front;
   begin Result := Pointer( list^.sentinel.next );
     end;
function enet_list_back;
   begin Result := Pointer( list^.sentinel.previous );
     end;

end.
