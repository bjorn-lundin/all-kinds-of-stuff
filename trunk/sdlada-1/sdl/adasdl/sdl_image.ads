
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
--  The SDL image library was written in C  by:                      --
--  Sam Lantinga - www.libsld.org                                    --
--  **************************************************************** --
--  **************************************************************** --
--  In order to help the Ada programmer, the comments in this file   --
--  are, in great extent, a direct copy of the original text in the  --
--  SDL mixer header files.                                          --
--  **************************************************************** --


--  A simple library to load images of various formats as SDL surfaces
with Interfaces.C.Strings;
with SDL.RWops; use SDL.RWops;
with SDL.Video; use SDL.Video;
with SDL.Error;
package SDL_Image is
   package V  renames SDL.Video;
   package RW renames SDL.RWops;
   package C  renames Interfaces.C;
   package CS renames Interfaces.C.Strings;
   --  Load an image from an SDL data source.
   --  The 'type' may be one of: "BMP", "GIF", "PNG", etc.

   --  If the image format supports a transparent pixel, SDL will set the
   --  colorkey for the surface.  You can enable RLE acceleration on the
   --  surface afterwards by calling:
   --  SetColorKey (image, SDL_RLEACCEL, image.format.colorkey);
   function LoadTyped_RW (src : RW.RWops_ptr;
                          freesrc : C.int;
                          the_type : CS.chars_ptr)
                          return V.Surface_ptr;
   pragma Import (C, LoadTyped_RW, "IMG_LoadTyped_RW");

   function LoadTyped_RW (src : RW.RWops_ptr;
                          freesrc : C.int;
                          the_type : String)
                          return V.Surface_ptr;
   pragma Inline (LoadTyped_RW);

   --  Convenience functions
   function Load (file : CS.chars_ptr) return V.Surface_ptr;
   pragma Import (C, Load, "IMG_Load");

   function Load (file : String) return V.Surface_ptr;
   pragma Inline (Load);

   function Load_RW (src : RW.RWops_ptr; freesrc : C.int) return V.Surface_ptr;
   pragma Import (C, Load_RW, "IMG_Load_RW");

   --  Invert the alpha of a surface for use with OpenGL
   --  This function is now a no-op, and only provided for backwards compatibility.
   function InvertAlpha (on : C.int) return C.int;
   pragma Import (C, InvertAlpha, "IMG_InvertAlpha");


   --  Functions to detect a file type, given a seekable source */
   function isBMP (src : RW.RWops_ptr) return C.int;
   pragma Import (C, isBMP, "IMG_isBMP");

   function isPPM (src : RW.RWops_ptr) return C.int;
   pragma Import (C, isPPM, "IMG_isPPM");

   function isPCX (src : RW.RWops_ptr) return C.int;
   pragma Import (C, isPCX, "IMG_isPCX");

   function isGIF (src : RW.RWops_ptr) return C.int;
   pragma Import (C, isGIF, "IMG_isGIF");

   function isJPG (src : RW.RWops_ptr) return C.int;
   pragma Import (C, isJPG, "IMG_isJPG");

   function isTIF (src : RW.RWops_ptr) return C.int;
   pragma Import (C, isTIF, "IMG_isTIF");

   function isPNG (src : RW.RWops_ptr) return C.int;
   pragma Import (C, isPNG, "IMG_isPNG");

   --  Individual loading functions
   function LoadBMP_RW (src : RW.RWops) return V.Surface_ptr;
   pragma Import (C, LoadBMP_RW, "IMG_LoadBMP_RW");

   function LoadPPM_RW (src : RW.RWops) return V.Surface_ptr;
   pragma Import (C, LoadPPM_RW, "IMG_LoadPPM_RW");

   function LoadPCX_RW (src : RW.RWops) return V.Surface_ptr;
   pragma Import (C, LoadPCX_RW, "IMG_LoadPCX_RW");

   function LoadGIF_RW (src : RW.RWops) return V.Surface_ptr;
   pragma Import (C, LoadGIF_RW, "IMG_LoadGIF_RW");

   function LoadJPG_RW (src : RW.RWops) return V.Surface_ptr;
   pragma Import (C, LoadJPG_RW, "IMG_LoadJPG_RW");

   function LoadTIF_RW (src : RW.RWops) return V.Surface_ptr;
   pragma Import (C, LoadTIF_RW, "IMG_LoadTIF_RW");

   function LoadPNG_RW (src : RW.RWops) return V.Surface_ptr;
   pragma Import (C, LoadPNG_RW, "IMG_LoadPNG_RW");

   function LoadTGA_RW (src : RW.RWops) return V.Surface_ptr;
   pragma Import (C, LoadTGA_RW, "IMG_LoadTGA_RW");

   --  We'll use SDL for reporting errors
   procedure SetError (fmt : CS.chars_ptr)
      renames SDL.Error.SetError;
   procedure Set_Error (fmt : String)
      renames SDL.Error.Set_Error;
   function GetError return CS.chars_ptr
      renames SDL.Error.GetError;
   function Get_Error return String
      renames SDL.Error.Get_Error;

end SDL_Image;
