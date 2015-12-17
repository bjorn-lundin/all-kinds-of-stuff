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
with Ada.Unchecked_Conversion;
with Interfaces.C;
with SDL2.Error;
--with SDL2.Video.Renderers;
package body SDL2.Video.Textures.Makers is
  package C renames Interfaces.C;
  
  procedure Create
     (Self      : in out Texture;
      Renderer : in SDL2.Video.Renderers.Renderer;
      Format   : in SDL2.Video.Pixel_Formats.Pixel_Format_Names;
      Kind     : in Kinds;
      Size     : in SDL2.Video.Windows.Sizes) is

      --  Convert the Pixel_Format_Name to an Unsigned_32 because the compiler is changing the value somewhere along
      --  the lines from the start of this procedure to calling SDL_Create_Texture.
      function To_Unsigned32 is new Ada.Unchecked_Conversion (Source => SDL2.Video.Pixel_Formats.Pixel_Format_Names,
                                                              Target => Interfaces.Unsigned_32);

      function SDL_Create_Texture
        (R      : in SDL2.Renderer_Pointer;
         Format : in Interfaces.Unsigned_32;
         Kind   : in Kinds;
         W, H   : in C.int) return Texture_Pointer ;
         pragma Import (C, SDL_Create_Texture, "SDL_CreateTexture");

   begin
      Self.Pointer := SDL_Create_Texture (Renderer.Get_Pointer ,
                                          To_Unsigned32 (Format),
                                          Kind,
                                          C.int (Size.Width),
                                          C.int (Size.Height));
      if Self.Pointer = null then
         raise Texture_Error with SDL2.Error.Get;
      end if;
      Self.Size         := Size;
      Self.Pixel_Format := Format;
   end Create;

   procedure Create
     (Self      : in out Texture;
      Renderer : in SDL2.Video.Renderers.Renderer;
      Surface  : in SDL2.Video.Surfaces.Surface) is

      function SDL_Create_Texture_Form_Surface (R : in  SDL2.Renderer_Pointer;
                                                S : in  SDL2.Video.Surfaces.Surface_Pointer)
                                                return Texture_Pointer ;
      pragma Import (C, SDL_Create_Texture_Form_Surface, "SDL_CreateTextureFromSurface");
   begin
      Self.Pointer := SDL_Create_Texture_Form_Surface(Renderer.Get_Pointer,
                                                       Surface.Get_Pointer);
                                                       
      pragma compile_Time_Warning (True, "query for size and pixel format ");                                                       

      if Self.Pointer = null then
         raise Texture_Error with SDL2.Error.Get;
      end if;
   end Create;
  
end SDL2.Video.Textures.Makers;
