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
   
--   function Make (S : in Surface_Pointer; Owns : in Boolean) return Surface is
--   begin
--      return R : Surface := (Ada.Finalization.Limited_Controlled with Internal => S, Owns => Owns) do
--         null;
--      end return;
--   end Make;
   
  procedure Create(Self  : in out Surface;
                   Font  : in     SDL2.TTF.Font;
                   Color : in     SDL2.Video.Palettes.Colour ;
                   Text  : in     String ) is
    
   --  SDL_Surface *TTF RenderText_Blended(TTF_Font *font, const char *text, SDL_Color fg)  
    function RenderText_Blended(Font_Ptr    : SDL.TTF.C_Font_Access ;  
                               Text        : Interfaces.C.Strings.Chars_Ptr; 
                               Fore_Ground : SDL.Video.Palettes.Colour) return Surface_Pointer;
    pragma Import(C,RenderText_Blended, "TTF_RenderText_Blended");    
--    pragma Import(C,RenderText_Blended, "TTF_RenderText_Solid");    
    C_Text : Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.New_String(Text);
    Surface_Ptr : Surface_Pointer := null;
  begin
    Surface_Ptr := RenderText_Blended(Font_Ptr    => Font.Ptr,  
                                      Text        => C_Text,
                                      Fore_Ground => Color); 
    Interfaces.C.Strings.Free(C_Text);
    Self.Create(Internal => Surface_Ptr, Owns => True);
  end Create;
  
  
-- Finalize will do this  
--  procedure Destroy(Surface  : in out SDL.Video.Surfaces.Surface) is
--    procedure SDL_FreeSurface ( Surface_Ptr : in Surface_Pointer);
--    pragma Import (C, SDL_FreeSurface, "SDL_FreeSurface");
--  begin
--    SDL_FreeSurface(Surface.Internal);
--  end Destroy;  
  
  --SDL_FreeSurface                   
   
end SDL2.Video.Surfaces;
