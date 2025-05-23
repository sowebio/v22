--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.Generic_Source.Get_Text             Luebeck            --
--  Interface                                      Winter, 2004       --
--                                                                    --
--                                Last revision :  11:37 13 Oct 2007  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--
--
--  Get_Text -- Skip a text in the source
--
--     Code   - The source code
--     Text   - The text
--     Got_It - Set to false if Text is not there
--     Map    - Character equivalence map
--
--  This  procedure  skips Text in Code. The parameter Map specifies the
--  character  equivalence.  A  character  in the source and in Text are
--  equivalent  when  they  are  equivalent  in  Map.  The default value
--  considers  all  characters  distinct. To have case-insensitive match
--  one can use Ada.Strings.Maps.Constants.Lower_Case_Map.
--
with Ada.Strings.Maps;  use Ada.Strings.Maps;

generic
procedure Parsers.Generic_Source.Get_Text
          (  Code   : in out Source_Type;
             Text   : String;
             Got_It : out Boolean;
             Map    : Character_Mapping := Identity
          );
