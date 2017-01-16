
-- ----------------------------------------------------------------- --
--                AdaSDL                                             --
--                Thin binding to Simple Direct Media Layer          --
--                Copyright (C) 2000-2012  A.M.F.Vargas              --
--                Antonio M. M. Ferreira Vargas                      --
--                Manhente - Barcelos - Portugal                     --
--                http://adasdl.sourceforge.net                      --
--                E-mail: amfvargas@gmail.com                        --
-- ----------------------------------------------------------------- --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-- ----------------------------------------------------------------- --

--  **************************************************************** --
--  The SDL mixer library was written in C  by:                      --
--  Sam Lantinga - www.libsld.org                                    --
--  **************************************************************** --

package body SDL_Mixer is

   --  Mode strings.
   --  "rb"
   Mode_RB     : aliased  C.char_array := C.To_C ("rb");
   Mode_RB_Ptr : constant C.Strings.chars_ptr := C.Strings.To_Chars_Ptr (Mode_RB'Access);

   use type C.int;

   --  ======================================
   function LoadWAV (file : CS.chars_ptr) return Chunk_ptr is
   begin
      return LoadWAV_RW (RW.RWFromFile (file, Mode_RB_Ptr), 1);
   end LoadWAV;

   --  ======================================
   function Load_WAV (file : String) return Chunk_ptr is
   begin
      return LoadWAV_RW (RW.RW_From_File (file, "rb"), 1);
   end Load_WAV;

   --  ======================================
   function Load_MUS (file : String) return Music_ptr is
      File_CString : aliased C.char_array := C.To_C (file);
   begin
      return LoadMUS (CS.To_Chars_Ptr (File_CString'Unchecked_Access));
   end Load_MUS;

   --  ======================================
   function PlayChannel (
      channel : C.int;
      chunk   : Chunk_ptr;
      loops   : C.int)
      return C.int
   is
   begin
      return PlayChannelTimed (channel, chunk, loops, -1);
   end PlayChannel;

   --  ======================================
   procedure PlayChannel (
      channel : C.int;
      chunk   : Chunk_ptr;
      loops   : C.int)
   is
   begin
      PlayChannelTimed (channel, chunk, loops, -1);
   end PlayChannel;

   --  ======================================
   function FadeInChannel (
      channel : C.int;
      chunk   : Chunk_ptr;
      loops   : C.int;
      ms      : C.int)
      return C.int
   is
   begin
      return FadeInChannelTimed (channel, chunk, loops, ms, -1);
   end FadeInChannel;

   --  ======================================
   function Set_Music_CMD (command : String) return Integer is
   begin
      if command = "" then
         return Integer (SetMusicCMD (CS.Null_Ptr));
      else
         declare
            Cmd_CString : aliased C.char_array := C.To_C (command);
         begin
            return
              Integer (SetMusicCMD (CS.To_Chars_Ptr (Cmd_CString'Unchecked_Access)));
         end;
      end if;
   end Set_Music_CMD;

   --  ======================================
   procedure Set_Music_CMD (command : String) is
   begin
      if command = "" then
         SetMusicCMD (CS.Null_Ptr);
      else
         declare
            Cmd_CString : aliased C.char_array := C.To_C (command);
         begin
            SetMusicCMD (CS.To_Chars_Ptr (Cmd_CString'Unchecked_Access));
         end;
      end if;
   end Set_Music_CMD;
   --  ======================================
end SDL_Mixer;
