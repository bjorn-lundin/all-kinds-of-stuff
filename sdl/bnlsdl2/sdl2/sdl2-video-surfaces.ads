--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2014-2015 Luke A. Guest
--
--  This software is provided 'as-is', without any express or implied
--  warranty. In no event will the authors be held liable for any damages
--  arising from the use of this software.
--
--  Permission is granted to anyone to use this software for any purpose,
--  including commercial applications, and to alter it and redistribute it
--  freely, subject to the following restrictions:
--
--     1. The origin of this software must not be misrepresented; you must not
--     claim that you wrote the original software. If you use this software
--     in a product, an acknowledgment in the product documentation would be
--     appreciated but is not required.
--
--     2. Altered source versions must be plainly marked as such, and must not be
--     misrepresented as being the original software.
--
--     3. This notice may not be removed or altered from any source
--     distribution.
--------------------------------------------------------------------------------------------------------------------
--   SDL2.Video.Surfaces
--
--  Render surface abstraction.
--------------------------------------------------------------------------------------------------------------------
with Ada.Finalization;
with SDL2.TTF;
with SDL2.Video.Palettes;

package SDL2.Video.Surfaces is

   type Surface_Pointer is access all Interfaces.C.Int with
   Convention => C;

   type Surface is new Ada.Finalization.Limited_Controlled with private;
   function Get_Internal(Self : in Surface) return Surface_Pointer;

   Null_Surface : constant Surface;

   overriding
   procedure Finalize (Self : in out Surface);
   
   procedure Create( Self     : in out Surface;
                     Internal : Surface_Pointer;
                     Owns     : Boolean)  ;
                     
  procedure Create_Solid(Self  : in out Surface;
                         Font  : in     SDL2.TTF.Font;
                         Color : in     SDL2.Video.Palettes.RGB_Colour ;
                         Text  : in     String ) ;

  procedure Create_Blended(Self  : in out Surface;
                           Font  : in     SDL2.TTF.Font;
                           Color : in     SDL2.Video.Palettes.RGB_Colour ;
                           Text  : in     String ) ;

  procedure Create_Shaded(Self       : in out Surface;
                          Font       : in     SDL2.TTF.Font;
                          Color      : in     SDL2.Video.Palettes.RGB_Colour ;
                          Back_Color : in     SDL2.Video.Palettes.RGB_Colour ;
                          Text       : in     String ) ;
   
private
   type Surface is new Ada.Finalization.Limited_Controlled with
      record
         Internal : Surface_Pointer := null;
         Owns     : Boolean         := True;
      end record;


   Null_Surface : constant Surface := (Ada.Finalization.Limited_Controlled with
                                       Internal => null,
                                       Owns     => True);

end SDL2.Video.Surfaces;
