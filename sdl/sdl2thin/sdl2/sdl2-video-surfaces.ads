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


  type Surface is new Ada.Finalization.Controlled with private;

  function Get_Pointer(Self : in Surface) return Surface_Pointer;

  overriding
  procedure Finalize (Self : in out Surface);
  
  procedure Create( Self     : in out Surface;
                    Pointer  : Surface_Pointer;
                    Owns     : Boolean)  ;
                     
  procedure Create_Solid(Self  : in out Surface;
                         Font  : in     SDL2.TTF.Font;
                         Color : in     SDL2.Video.Palettes.RGB_Color ;
                         Text  : in     String ) ;

  procedure Create_Blended(Self  : in out Surface;
                           Font  : in     SDL2.TTF.Font;
                           Color : in     SDL2.Video.Palettes.RGB_Color ;
                           Text  : in     String ) ;

  procedure Create_Shaded(Self       : in out Surface;
                          Font       : in     SDL2.TTF.Font;
                          Color      : in     SDL2.Video.Palettes.RGB_Color ;
                          Back_Color : in     SDL2.Video.Palettes.RGB_Color ;
                          Text       : in     String ) ;
   
  procedure Destroy(Self  : in out Surface) ;
   
  Null_Surface : constant Surface ;
   
private
  type Surface is new Ada.Finalization.Controlled with record
        Pointer : Surface_Pointer := null;
        Owner   : Boolean         := True;
  end record;
                                       
  Null_Surface : constant Surface := (Ada.Finalization.Controlled with
                                       Pointer => null,
                                       Owner   => True);
                                       
                                       
end SDL2.Video.Surfaces;
