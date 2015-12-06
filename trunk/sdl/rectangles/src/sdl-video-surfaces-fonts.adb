
with Interfaces.C.Strings ;
with SDL.Video.Surfaces.Internal_Makers;

package body SDL.Video.Surfaces.Fonts is

  procedure Create(Surface  : in out SDL.Video.Surfaces.Surface;
                   Font     : in     SDL.TTF.Font;
                   Color    : in     SDL.Video.Palettes.Colour ;
                   Text     : in     String ) is
    
   --  SDL_Surface *TTF RenderText_Blended(TTF_Font *font, const char *text, SDL_Color fg)  
    function RenderText_Blended(Font_Ptr    : SDL.TTF.C_Font_Access ;  
                               Text        : Interfaces.C.Strings.Chars_Ptr; 
                               Fore_Ground : SDL.Video.Palettes.Colour) return SDL.C_Pointers.Surface_Pointer;
    pragma Import(C,RenderText_Blended, "TTF_RenderText_Blended");    
--    pragma Import(C,RenderText_Blended, "TTF_RenderText_Solid");    
    C_Text : Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.New_String(Text);
    Surface_Ptr : SDL.C_Pointers.Surface_Pointer := null;
  begin
    Surface_Ptr := RenderText_Blended(Font_Ptr    => Font.Ptr,  
                                      Text        => C_Text,
                                      Fore_Ground => Color); 
    Interfaces.C.Strings.Free(C_Text);
    SDL.Video.Surfaces.Create(Self => Surface, Internal => Surface_Ptr, Owns => True);
  end Create;
  
  procedure Destroy(Surface  : in out SDL.Video.Surfaces.Surface) is
    procedure SDL_FreeSurface ( Surface_Ptr : in SDL.C_Pointers.Surface_Pointer);
    pragma Import (C, SDL_FreeSurface, "SDL_FreeSurface");
  begin
    SDL_FreeSurface(Surface.Internal);
  end Destroy;  
  
  --SDL_FreeSurface                   
  
  
end SDL.Video.Surfaces.Fonts ;



 