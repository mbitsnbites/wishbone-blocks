----------------------------------------------------------------------------------------------------
-- Copyright (c) 2020 Marcus Geelnard
--
-- This software is provided 'as-is', without any express or implied warranty. In no event will the
-- authors be held liable for any damages arising from the use of this software.
--
-- Permission is granted to anyone to use this software for any purpose, including commercial
-- applications, and to alter it and redistribute it freely, subject to the following restrictions:
--
--  1. The origin of this software must not be misrepresented; you must not claim that you wrote
--     the original software. If you use this software in a product, an acknowledgment in the
--     product documentation would be appreciated but is not required.
--
--  2. Altered source versions must be plainly marked as such, and must not be misrepresented as
--     being the original software.
--
--  3. This notice may not be removed or altered from any source distribution.
----------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------
-- Various helpers for test benches. ONLY use this for test code.
----------------------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package test is
  function to_vector(x: integer; size: integer) return std_logic_vector;
  function to_string(x: integer) return string;
  function to_string(x: std_logic) return string;
  function to_string(x: std_logic_vector) return string;
end package;

package body test is
  function to_vector(x: integer; size: integer) return std_logic_vector is
  begin
    return std_logic_vector(to_unsigned(x, size));
  end function;

  function to_string(x: integer) return string is
  begin
    return integer'image(x);
  end function;

  function to_string(x: std_logic) return string is
  begin
    return "" & std_logic'image(x)(2);
  end function;

  function to_string(x: std_logic_vector) return string is
    variable v_b : string (1 to x'length) := (others => NUL);
    variable v_stri : integer := 1;
  begin
    for i in x'range loop
      v_b(v_stri) := std_logic'image(x(i))(2);
      v_stri := v_stri + 1;
    end loop;
    return v_b;
  end function;
end package body;
