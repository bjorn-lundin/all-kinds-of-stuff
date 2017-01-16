program Chapter7_SDL2;

uses SDL2, SDL2_ttf;

var
sdlSurface1 : PSDL_Surface;
ttfFont : PTTF_Font;
sdlColor1, sdlColor2 : PSDL_Color;
sdlWindow1 : PSDL_Window;
sdlRenderer : PSDL_Renderer;
sdlTexture1 : PSDL_Texture;

begin

  //initilization of video subsystem
  if SDL_Init( SDL_INIT_VIDEO ) < 0 then HALT;

  sdlWindow1 := SDL_CreateWindow( 'Window1', 50, 50, 500, 500, SDL_WINDOW_SHOWN );
  if sdlWindow1 = nil then HALT;

  sdlRenderer := SDL_CreateRenderer( sdlWindow1, -1, 0 );
  if sdlRenderer = nil then HALT;


  //initialization of TrueType font engine and loading of a font
  if TTF_Init = -1 then HALT;
  ttfFont := TTF_OpenFont( 'C:\WINDOWS\fonts\Arial.ttf', 40 );
  TTF_SetFontStyle( ttfFont, TTF_STYLE_UNDERLINE or TTF_STYLE_ITALIC );
  TTF_SetFontOutline( ttfFont, 1 );
  TTF_SetFontHinting( ttfFont, TTF_HINTING_NORMAL );

  //get memory for color manipulations and set new values
  new(sdlColor1);
  sdlColor1^.r := 255; sdlColor1^.g := 0; sdlColor1^.b := 0;
  new(sdlColor2);
  sdlColor2^.r := 0; sdlColor2^.g := 255; sdlColor2^.b := 255;

  //rendering a text to a SDL_Surface
  sdlSurface1 := TTF_RenderText_Shaded( ttfFont, 'Hello World!', sdlColor1^, sdlColor2^ );

  //convert SDL_Surface to SDL_Texture
  sdlTexture1 := SDL_CreateTextureFromSurface( sdlRenderer, sdlSurface1 );

  //rendering of the texture
  SDL_RenderCopy( sdlRenderer, sdlTexture1, nil, nil );
  SDL_RenderPresent( sdlRenderer );
  SDL_Delay( 5000 );

  //cleaning procedure
  dispose(sdlColor1);
  dispose(sdlColor2);
  TTF_CloseFont(ttfFont);
  TTF_Quit;

  SDL_FreeSurface( sdlSurface1 );
  SDL_DestroyTexture( sdlTexture1 );
  SDL_DestroyRenderer( sdlRenderer );
  SDL_DestroyWindow ( sdlWindow1 );

  //shutting down video subsystem
  SDL_Quit;

end.
