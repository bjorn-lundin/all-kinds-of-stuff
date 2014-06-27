with AVR;                          use AVR;
with AVR.UART;
with LCD; use LCD;

with AVR.Wait;
procedure LCD_Controller is


--   procedure Wait_10ms is new AVR.Wait.Generic_Wait_Usecs
--     (Crystal_Hertz => 16_000_000,
--      Micro_Seconds => 10_000);

    -- protocol
    -- stx-row-pos-val-etx
    -- if val=NUL => clear_screen
    -- if val=SO => Display_off
    -- if val=SI => Display_on
--    
--    type State_Type is (Stx, Row, Pos, Val, Etx);
--    Next_State : State_Type := Stx;
--    Line       : Line_Position := Line_Position'First;
--    Col        : Char_Position := Char_Position'First;
--    Value      : Character := ' ';
--    C          : Character := ' ';

   procedure Wait_1ms is new AVR.Wait.Generic_Wait_Usecs
     (Crystal_Hertz => 16_000_000,
      Micro_Seconds => 1_000);


begin
    LCD.Init;
--    LCD.Clear_Screen;
    
    UART.Init(25) ; -- 38400 baud on 16MHz

--    loop
      
--      LCD.GotoXY(1, 1);
--      LCD.Put("Hej");
--      UART.Put("Hej");
--      UART.New_Line;
--      LCD.GotoXY(1, 2);
--      LCD.Put("Betfair");
--      UART.Put("Betfair");
--      UART.New_Line;
    loop
      LCD.GotoXY(1, 1);
      LCD.Put('-');
      LCD.GotoXY(1, 2);
      LCD.Put("-----------------");
      UART.Put("Running");
      UART.New_Line;
      for i in 1..1_000 loop
        Wait_1ms;  
      end loop;
    end loop;


    
    
--    loop
--        C := UART.Get;
--        --keep state
--        case Next_State is
--            when Stx =>
--                Next_State := Row;
--            when Row =>
--               Next_State := Pos;
--                case C is
--                    when '0'       => Line :=  0 + 1 ;
--                    when '1'       => Line :=  1 + 1 ;
--                    when '2'       => Line :=  2 + 1 ;
--                    when others    => Line := 1;
--                end case;
--                    
--            when Pos =>
--                Next_State := Val;
--                case C is
--                    when '0'       => Col :=  0 + 1 ;
--                    when '1'       => Col :=  1 + 1 ;
--                    when '2'       => Col :=  2 + 1 ;
--                    when '3'       => Col :=  3 + 1 ;
--                    when '4'       => Col :=  4 + 1 ;
--                    when '5'       => Col :=  5 + 1 ;
--                    when '6'       => Col :=  6 + 1 ;
--                    when '7'       => Col :=  7 + 1 ;
--                    when '8'       => Col :=  8 + 1 ;
--                    when '9'       => Col :=  9 + 1 ;
--                    when 'A' | 'a' => Col := 10 + 1 ;
--                    when 'B' | 'b' => Col := 11 + 1 ;
--                    when 'C' | 'c' => Col := 12 + 1 ;
--                    when 'D' | 'd' => Col := 13 + 1 ;
--                    when 'E' | 'e' => Col := 14 + 1 ;
--                    when 'F' | 'f' => Col := 15 + 1 ;
--                    when others    => Col := 1;
--                end case;
--               
--            when Val=>
--                Next_State := Etx;
--                Value := C; 
--            when Etx => -- Execute The Action 
--                Next_State := Stx;
--                case Value is
--                    when Ascii.Nul => LCD.Clear_Screen;
--                    when ' ' .. '~' =>    -- writable chars
--                        LCD.GotoXY(Col, Line);
--                        LCD.Put(Value);
--                    when others => null;
--                end case;
--        end case;
--    end loop;
end LCD_Controller;