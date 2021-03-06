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
with Interfaces.C.Strings;
package body SDL2.Video.Surfaces is

   overriding
   procedure Finalize (Self : in out Surface) is
      --        procedure SDL_Destroy_Renderer (R : in Renderer_Pointer) with
      --          Import        => True,
      --          Convention    => C,
      --          External_Name => "SDL_DestroyRenderer";
   begin
      if Self.Internal /= null and then Self.Owns then
         --           SDL_Destroy_Renderer (Self.Internal);

         Self.Internal := null;
      end if;
   end Finalize;

   function Get_Internal (Self : in Surface) return Surface_Pointer is
   begin
      return Self.Internal;
   end Get_Internal;
   
   
   -- bnl start
   procedure Create( Self     : in out Surface;
                     Internal : Surface_Pointer;
                     Owns     : Boolean)  is
                   
   begin
      Self.Internal := Internal;
      Self.Owns     := Owns;
   end Create;
   -- bnl stop
   
  procedure Create_Solid(Self  : in out Surface;
                         Font  : in     SDL2.TTF.Font;
                         Color : in     SDL2.Video.Palettes.RGB_Colour ;
                         Text  : in     String ) is    
    function RenderText_Solid(Font_Ptr    : SDL2.TTF.Font_Pointer ;  
                               Text        : Interfaces.C.Strings.Chars_Ptr; 
                               Fore_Ground : SDL2.Video.Palettes.RGB_Colour) return Surface_Pointer;
    pragma Import(C,RenderText_Solid, "TTF_RenderText_Solid");    
    C_Text : Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.New_String(Text);
    Surface_Ptr : Surface_Pointer := null;
  begin
    Surface_Ptr := RenderText_Solid(Font_Ptr    => Font.Ptr,  
                                    Text        => C_Text,
                                    Fore_Ground => Color); 
    Interfaces.C.Strings.Free(C_Text);
    Self.Create(Internal => Surface_Ptr, Owns => True);
  end Create_Solid;
  
  procedure Create_Blended(Self  : in out Surface;
                           Font  : in     SDL2.TTF.Font;
                           Color : in     SDL2.Video.Palettes.RGB_Colour ;
                           Text  : in     String ) is    
    function RenderText_Blended(Font_Ptr    : SDL2.TTF.Font_Pointer ;  
                               Text        : Interfaces.C.Strings.Chars_Ptr; 
                               Fore_Ground : SDL2.Video.Palettes.RGB_Colour) return Surface_Pointer;
    pragma Import(C,RenderText_Blended, "TTF_RenderText_Blended");    
    C_Text : Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.New_String(Text);
    Surface_Ptr : Surface_Pointer := null;
  begin
    Surface_Ptr := RenderText_Blended(Font_Ptr    => Font.Ptr,  
                                      Text        => C_Text,
                                      Fore_Ground => Color); 
    Interfaces.C.Strings.Free(C_Text);
    Self.Create(Internal => Surface_Ptr, Owns => True);
  end Create_Blended;

  procedure Create_Shaded(Self  : in out Surface;
                          Font  : in     SDL2.TTF.Font;
                          Color : in     SDL2.Video.Palettes.RGB_Colour ;
                          Back_Color : in     SDL2.Video.Palettes.RGB_Colour ;
                          Text  : in     String ) is
    function RenderText_Shaded(Font_Ptr    : SDL2.TTF.Font_Pointer ;  
                               Text        : Interfaces.C.Strings.Chars_Ptr; 
                               Fore_Ground : SDL2.Video.Palettes.RGB_Colour;
                               Back_Ground : SDL2.Video.Palettes.RGB_Colour ) return Surface_Pointer;
    pragma Import(C,RenderText_Shaded, "TTF_RenderText_Shaded");    
    C_Text : Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.New_String(Text);
    Surface_Ptr : Surface_Pointer := null;
  begin
    Surface_Ptr := RenderText_Shaded(Font_Ptr    => Font.Ptr,  
                                     Text        => C_Text,
                                     Fore_Ground => Color,
                                     Back_Ground => Back_Color); 
    Interfaces.C.Strings.Free(C_Text);
    Self.Create(Internal => Surface_Ptr, Owns => True);
  end Create_Shaded;


  
-- Finalize will do this  
--  procedure Destroy(Surface  : in out SDL2.Video.Surfaces.Surface) is
--    procedure SDL_FreeSurface ( Surface_Ptr : in Surface_Pointer);
--    pragma Import (C, SDL_FreeSurface, "SDL_FreeSurface");
--  begin
--    SDL_FreeSurface(Surface.Internal);
--  end Destroy;  
  
  --SDL_FreeSurface                   
   
end SDL2.Video.Surfaces;
