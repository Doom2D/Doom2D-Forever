unit ENet_List;

{
  ENet - Reliable UDP networking library

  Delphi 7 DLL header: ENet_List.pas
  Copyright (c) 2014 Dmitry D. Chernov aka Black Doomer

  Original file: list.h
  Copyright (c) 2002-2014 Lee Salzman

  Version 1 for 1.3.12: 16.08.2014

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
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
