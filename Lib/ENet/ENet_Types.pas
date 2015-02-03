unit ENet_Types;

{
  ENet - Reliable UDP networking library

  Delphi 7 DLL header: ENet_Types.pas
  Copyright (c) 2014 Dmitry D. Chernov aka Black Doomer

  Original file: types.h
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
