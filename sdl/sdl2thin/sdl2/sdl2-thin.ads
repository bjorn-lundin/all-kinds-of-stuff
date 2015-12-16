
with Interfaces.C;          use Interfaces.C;
with Interfaces.C.Strings;  use Interfaces.C.Strings;
with SDL2.Video.Rectangles; use SDL2.Video.Rectangles;
with SDL2.Video.Colors;     use SDL2.Video.Colors;
with SDL2.Video.Windows;   
with SDL2.Video.Renderers;  


package SDL2.Thin is


--  function SDL_Init (Flags : in SDL2.Init_Flags := SDL2.Enable_Everything) return Int ;
--  pragma Import(C, SDL_Init, "SDL_Init");

-- start font
  function TTF_Init return Int;
  pragma Import(C,TTF_Init,"TTF_Init");
  
 --Font *TTF_OpenFont( const char *file, int ptsize)
  function TTF_OpenFont(Filename   : Chars_Ptr; 
                        Point_Size : Int ) return Font_Pointer ;
  pragma Import (C, TTF_OpenFont, "TTF_OpenFont");
                        
 -- void TTF_CloseFont(TTF_Font* font );
  procedure TTF_Close_Font(Font_Ptr : in Font_Pointer);
  pragma Import (C, TTF_Close_Font, "TTF_CloseFont");
  
  procedure TTF_Quit ;
  pragma Import(C,TTF_Quit,"TTF_Quit");

-- start windows

  function SDL_Create
    (Title      : Chars_Ptr;
     X, Y, W, H : in Int;
     F          : in  SDL2.Video.Windows.Window_Flags) return Window_Pointer ;
  pragma Import(C, SDL_Create, "SDL_CreateWindow");

  -- start renderer           
  function SDL_Render_Set_Logical_Size (R : in Renderer_Pointer;
                                        W : Int;
                                        H : Int ) return Int;
  pragma Import(C, SDL_Render_Set_Logical_Size, "SDL_RenderSetLogicalSize");
    
  function SDL_Render_Clear (R : in Renderer_Pointer) return Int ;
  pragma Import(C, SDL_Render_Clear, "SDL_RenderClear");
    
  function SDL_Render_Copy
    (R         : in Renderer_Pointer;
     T         : in Texture_Pointer;
     Src       : in Rectangle_Pointer;
     Dest      : in Rectangle_Pointer) return Int ;
  pragma Import(C, SDL_Render_Copy, "SDL_RenderCopy");
   
  procedure SDL_Render_Present (R : in Renderer_Pointer) ;
  pragma Import(C, SDL_Render_Present, "SDL_RenderPresent");
    
--  function TTF_RenderText_Blended(Font_Ptr    : Font_Pointer ;  
--                                  Text        : Chars_Ptr; 
--                                  Fore_Ground : RGB_Color) return Surface_Pointer;
--  pragma Import(C,TTF_RenderText_Blended, "TTF_RenderText_Blended");    

--  function TTF_RenderText_Solid(Font_Ptr      : Font_Pointer ;  
--                                  Text        : Chars_Ptr; 
--                                  Fore_Ground : RGB_Color) return Surface_Pointer;
--  pragma Import(C,TTF_RenderText_Solid, "TTF_RenderText_Solid");   
  
 -- function TTF_RenderText_Shaded(Font_Ptr     : Font_Pointer ;  
 --                                 Text        : Chars_Ptr; 
 --                                 Fore_Ground : RGB_Color;
 --                                 Back_Ground : RGB_Color) return Surface_Pointer;
 -- pragma Import(C,TTF_RenderText_Shaded, "TTF_RenderText_Shaded");    

  function SDL_Create_Texture_From_Surface (R : in  Renderer_Pointer;
                                            S : in  Surface_Pointer)
                                            return Texture_Pointer ;
  pragma Import (C, SDL_Create_Texture_From_Surface, "SDL_CreateTextureFromSurface");
     
--  procedure SDL_Free_Surface(S : in Surface_Pointer);
--  pragma Import (C, SDL_Free_Surface, "SDL_FreeSurface");
  
--  function SDL_Query_Texture (T       : Texture_Pointer;
--                              F       : access Long ;
--                              A, W, H : access Int) return Int ;
--  pragma Import(C,SDL_Query_Texture, "SDL_QueryTexture");   
    
  function SDL_Create_Renderer (W     : in Window_Pointer; 
                                Index : in Int; 
                                Flags : in SDL2.Video.Renderers.Renderer_Flags)
                                  return Renderer_Pointer ;
  pragma Import(C,SDL_Create_Renderer, "SDL_CreateRenderer");   

  function SDL_Set_Render_Draw_Color
      (R                       : in Renderer_Pointer;
       Red, Green, Blue, Alpha : in Int) return Int ;
  pragma Import(C,SDL_Set_Render_Draw_Color, "SDL_SetRenderDrawColor");   
    
--  function SDL_Get_Error return Chars_Ptr ;
--  pragma Import(C,SDL_Get_Error, "SDL_GetError");
  
  
  
  ------------------------------------------------------------

end SDL2.Thin;