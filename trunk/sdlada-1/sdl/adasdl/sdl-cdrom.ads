
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
--  This is an Ada binding to SDL ( Simple DirectMedia Layer from    --
--  Sam Lantinga - www.libsld.org )                                  --
--  **************************************************************** --
--  In order to help the Ada programmer, the comments in this file   --
--  are, in great extent, a direct copy of the original text in the  --
--  SDL header files.                                                --
--  **************************************************************** --

with Interfaces.C.Strings;
with SDL.Types; use SDL.Types;
package SDL.Cdrom is
   package CS renames Interfaces.C.Strings;

   --  The maximum number of CDROM tracks on a disk.
   MAX_TRACKS : constant := 99;

   --  the types of CD-ROM track possible
   AUDIO_TRACK : constant := 16#00#;
   DATA_TRACK  : constant := 16#04#;


   type CDstatus is new C.int;
   --  The possible states which a CD-ROM drive can be
   TRAYEMPTY : constant :=  0;
   STOPPED   : constant :=  1;
   PLAYING   : constant :=  2;
   PAUSED    : constant :=  3;
   ERROR     : constant := -1;

   --  Given a status, returns true if there's a disk in the drive
   --  #define CD_INDRIVE(status)        ((int)status > 0)
   function INDRIVE (status : CDstatus) return Boolean;
   pragma Inline (INDRIVE);

   type CDtrack is
      record
         id       : Uint8;  --  Track number
         the_type : Uint8;  --  Data or audio track
         unused   : Uint16;
         lenght   : Uint32; --  Length, in frames, of this track
         offset   : Uint32; --  Offset, in frames, from start of disk
      end record;
   pragma Convention (C, CDtrack);

   type track_Array is
      array (C.int range 0 .. MAX_TRACKS) of CDtrack;
   pragma Convention (C, track_Array);
   --  This structure is only current as of the last call to CDStatus
   type CD is
      record
         id : C.int; --  Private drive identifier
         status : CDstatus; --  Current drive status
         --  The rest of this structure is only valid if
         --  there's a CD in drive
         numtracks : C.int;  --  Number of tracks on disk
         cur_track : C.int;  --  Current track position
         cur_frame : C.int;  --  Current frame offset within current track
         track     : track_Array;
      end record;
      pragma Convention (C, CD);

      type CD_ptr is access CD;
      pragma Convention (C, CD_ptr);

   --  Conversion functions from frames to Minute/Second/Frames
   --  and vice versa
   CD_FPS : constant := 75;

   procedure FRAMES_TO_MSF (
      frames : C.int;
      M : in out C.int;
      S : in out C.int;
      F : in out C.int);
   pragma Inline (FRAMES_TO_MSF);

   function MSF_TO_FRAMES (
      M : C.int;
      S : C.int;
      F : C.int) return C.int;
   pragma Inline (MSF_TO_FRAMES);

   --  CD-audio API functions:

   --  Returns the number of CD-ROM drives on the system, or -1 if
   --  SDL.Init has not been called with the INIT_CDROM flag.
   function CDNumDrives return C.int;
   pragma Import (C, CDNumDrives, "SDL_CDNumDrives");

   --  Returns a human-readable, system-dependent identifier for the CD-ROM.
   --  Example:
   --     "/dev/cdrom"
   --     "E:"
   --     "/dev/disk/ide/1/master"
   function CDName (drive : C.int) return CS.chars_ptr;
   pragma Import (C, CDName, "SDL_CDName");

   --  Opens a CD-ROM drive for access.  It returns a drive handle
   --  on success, or NULL if the drive was invalid or busy.  This
   --  newly opened CD-ROM becomes the default CD used when other
   --  CD functions are passed a NULL CD-ROM handle.
   --  Drives are numbered starting with 0.  Drive 0 is the system
   --  default CD-ROM.
   function CDOpen (drive : C.int) return CD_ptr;
   pragma Import (C, CDOpen, "SDL_CDOpen");

   --  This function returns the current status of the given drive.
   --  If the drive has a CD in it, the table of contents of the CD
   --  and current play position of the CD will be stored in the
   --  SDL_CD structure.
   function SDL_CDStatus (cdrom : CD_ptr) return CDstatus;
   pragma Import (C, SDL_CDStatus, "SDL_CDStatus");


   --  Play the given CD starting at 'start_track' and 'start_frame'
   --  for 'ntracks' tracks and 'nframes' frames.  If both 'ntrack'
   --  and 'nframe' are 0, play until the end of the CD.  This function
   --  will skip data tracks. This function should only be called after
   --  calling SDL_CDStatus() to get track information about the CD.
   --  For example:
   --       --  Play entire CD:
   --       if CD_INDRIVE(SDL_CDStatus(cdrom)) /= 0 then
   --               CDPlayTracks(cdrom, 0, 0, 0, 0);
   --       end if;
   --       --  Play last track:
   --       if CD_INDRIVE(SDL_CDStatus(cdrom)) /= 0 then
   --               CDPlayTracks(cdrom, cdrom.numtracks-1, 0, 0, 0);
   --       end if;
   --       --  Play first and second track and 10 seconds of third track:
   --       if CD_INDRIVE(SDL_CDStatus(cdrom)) /= 0 then
   --               CDPlayTracks(cdrom, 0, 0, 2, 10);
   --       end if;

   --  This function returns 0, or -1 if there was an error.
   function CDPlayTracks (
      cdrom       : CD_ptr;
      start_track : C.int;
      start_frame : C.int;
      ntracks     : C.int;
      nframes     : C.int)
      return C.int;
   pragma Import (C, CDPlayTracks, "SDL_CDPlayTracks");

   --  Play the given CD starting at 'start' frame for 'length'
   --  frames. It returns 0, or -1 if there was an error.
   function CDPlay (
      cdrom  : CD_ptr;
      start  : C.int;
      lenght : C.int)
      return C.int;
   pragma Import (C, CDPlay, "SDL_CDPlay");

   --  Pause play -- returns 0, or -1 on error
   function CDPause (cdrom : CD_ptr) return C.int;
   pragma Import (C, CDPause, "SDL_CDPause");

   --  Resume play -- returns 0, or -1 on error
   function CDResume (cdrom : CD_ptr) return C.int;
   pragma Import (C, CDResume, "SDL_CDResume");

   --  Stop play -- returns 0, or -1 on error
   function CDStop (cdrom : CD_ptr) return C.int;
   pragma Import (C, CDStop, "SDL_CDStop");

   --  Eject CD-ROM -- returns 0, or -1 on error
   function CDEject (cdrom : CD_ptr) return C.int;
   pragma Import (C, CDEject, "SDL_CDEject");

   --  Closes the handle for the CD-ROM drive
   procedure CDClose (cdrom : CD_ptr);
   pragma Import (C, CDClose, "SDL_CDClose");

end SDL.Cdrom;
