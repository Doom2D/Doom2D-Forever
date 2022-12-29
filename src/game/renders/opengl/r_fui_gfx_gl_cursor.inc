(* coded by Ketmar // Invisible Vector <ketmar@ketmar.no-ip.org>
 * Understanding is not required. Only obedience.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3 of the License ONLY.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *)
// ////////////////////////////////////////////////////////////////////////// //
// cursor (hi, Death Track!)
const curTexWidth = 32;
const curTexHeight = 32;
const curWidth = 17;
const curHeight = 23;

const cursorImg: array[0..curWidth*curHeight-1] of Byte = (
  2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  3,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  3,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  3,3,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,
  3,3,4,2,2,0,0,0,0,0,0,0,0,0,0,0,0,
  3,3,4,4,2,2,0,0,0,0,0,0,0,0,0,0,0,
  3,3,4,4,4,2,2,0,0,0,0,0,0,0,0,0,0,
  3,3,4,4,4,4,2,2,0,0,0,0,0,0,0,0,0,
  3,3,4,4,4,5,6,2,2,0,0,0,0,0,0,0,0,
  3,3,4,4,5,6,7,5,2,2,0,0,0,0,0,0,0,
  3,3,4,5,6,7,5,4,5,2,2,0,0,0,0,0,0,
  3,3,5,6,7,5,4,5,6,7,2,2,0,0,0,0,0,
  3,3,6,7,5,4,5,6,7,7,7,2,2,0,0,0,0,
  3,3,7,5,4,5,6,7,7,7,7,7,2,2,0,0,0,
  3,3,5,4,5,6,8,8,8,8,8,8,8,8,2,0,0,
  3,3,4,5,6,3,8,8,8,8,8,8,8,8,8,0,0,
  3,3,5,6,3,3,0,0,0,0,0,0,0,0,0,0,0,
  3,3,6,3,3,0,0,0,0,0,0,0,0,0,0,0,0,
  3,3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,
  3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
);
const cursorPal: array[0..9*4-1] of Byte = (
    0,  0,  0,  0,
    0,  0,  0, 92, // shadow
   85,255,255,255,
   85, 85,255,255,
  255, 85, 85,255,
  170,  0,170,255,
   85, 85, 85,255,
    0,  0,  0,255,
    0,  0,170,255
);


var
  curtexid: GLuint = 0;


procedure createCursorTexture ();
var
  tex, tpp: PByte;
  c: Integer;
  x, y: Integer;
begin
  if (curtexid <> 0) then exit; //begin glDeleteTextures(1, @curtexid); curtexid := 0; end;

  GetMem(tex, curTexWidth*curTexHeight*4);
  try
    FillChar(tex^, curTexWidth*curTexHeight*4, 0);

    // draw shadow
    for y := 0 to curHeight-1 do
    begin
      for x := 0 to curWidth-1 do
      begin
        if (cursorImg[y*curWidth+x] <> 0) then
        begin
          c := 1*4;
          tpp := tex+((y+1)*(curTexWidth*4)+(x+3)*4);
          tpp^ := cursorPal[c+0]; Inc(tpp);
          tpp^ := cursorPal[c+1]; Inc(tpp);
          tpp^ := cursorPal[c+2]; Inc(tpp);
          tpp^ := cursorPal[c+3]; Inc(tpp);
          tpp^ := cursorPal[c+0]; Inc(tpp);
          tpp^ := cursorPal[c+1]; Inc(tpp);
          tpp^ := cursorPal[c+2]; Inc(tpp);
          tpp^ := cursorPal[c+3]; Inc(tpp);
        end;
      end;
    end;

    // draw cursor
    for y := 0 to curHeight-1 do
    begin
      for x := 0 to curWidth-1 do
      begin
        c := cursorImg[y*curWidth+x]*4;
        if (c <> 0) then
        begin
          tpp := tex+(y*(curTexWidth*4)+x*4);
          tpp^ := cursorPal[c+0]; Inc(tpp);
          tpp^ := cursorPal[c+1]; Inc(tpp);
          tpp^ := cursorPal[c+2]; Inc(tpp);
          tpp^ := cursorPal[c+3]; Inc(tpp);
        end;
      end;
    end;

    glGenTextures(1, @curtexid);
    if (curtexid = 0) then raise Exception.Create('can''t create cursor texture');

    glBindTexture(GL_TEXTURE_2D, curtexid);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

    //GLfloat[4] bclr = 0.0;
    //glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR, bclr.ptr);

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, curTexWidth, curTexHeight, 0, GL_RGBA{gltt}, GL_UNSIGNED_BYTE, tex);
    glFinish();
  finally
    FreeMem(tex);
  end;
end;


procedure oglDrawCursorAt (msX, msY: Integer);
var
  sst: TSavedGLState;
begin
  //if (curtexid = 0) then createCursorTexture() else glBindTexture(GL_TEXTURE_2D, curtexid);
  sst := TSavedGLState.Create(true);
  try
    oglSetup2D(fuiScrWdt, fuiScrHgt);
    glBindTexture(GL_TEXTURE_2D, curtexid);
    // blend it
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_TEXTURE_2D);
    glDisable(GL_STENCIL_TEST);
    glDisable(GL_SCISSOR_TEST);
    glDisable(GL_LIGHTING);
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_CULL_FACE);
    // color and opacity
    glColor4f(1, 1, 1, 1.0);
    //Dec(msX, 2);
    glBegin(GL_QUADS);
      glTexCoord2f(0.0, 0.0); glVertex2i(msX, msY); // top-left
      glTexCoord2f(1.0, 0.0); glVertex2i(msX+curTexWidth, msY); // top-right
      glTexCoord2f(1.0, 1.0); glVertex2i(msX+curTexWidth, msY+curTexHeight); // bottom-right
      glTexCoord2f(0.0, 1.0); glVertex2i(msX, msY+curTexHeight); // bottom-left
    glEnd();
    //Inc(msX, 2);
    //glDisable(GL_BLEND);
    //glDisable(GL_TEXTURE_2D);
    //glColor4f(1, 1, 1, 1);
    //glBindTexture(GL_TEXTURE_2D, 0);
  finally
    sst.restore();
  end;
end;
