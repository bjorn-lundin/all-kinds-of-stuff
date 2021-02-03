------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2013, AdaCore                        --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with Ada.Streams;

with Aws.Net.Log;
with Aws.Response;
with Aws.Status;

with Aws.Net.Websocket;
with Aws.Config;

package Websock_Cb is

  use Ada.Streams;
  use Aws;

  function Hw_Cb (Request : Status.Data) return Response.Data;

  procedure W_Log
    (Direction : Net.Log.Data_Direction;
     Socket    : Net.Socket_Type'Class;
     Data      : Stream_Element_Array;
     Last      : Stream_Element_Offset);

  --  My WebSocket, just display the messages

  subtype Session_Id_Type is String (1 .. Config.Session_Id_Length);
  type Object is new Aws.Net.Websocket.Object with private;

  function Create
    (Socket  : Net.Socket_Access;
     Request : Status.Data) return Net.Websocket.Object'Class;

  overriding procedure On_Message (Socket : in out Object; Message : String);
  --  Message received from the server

  overriding procedure On_Open (Socket : in out Object; Message : String);
  --  Open event received from the server

  overriding procedure On_Close (Socket : in out Object; Message : String);
  --  Close event received from the server

  overriding procedure On_Error (Socket : in out Object; Message : String);

  function To_String(Socket : Object) return String ; --bnl
  function Get_Session_Id(Socket : Object) return Session_Id_Type;
  procedure Set_Session_Id(Socket : in out Object; Sid : in session_Id_Type);


private

  type Object is new Net.Websocket.Object with record
    C : Natural := 0;
    Sid : Session_Id_Type := (others =>' ');
  end record;

end Websock_Cb;
