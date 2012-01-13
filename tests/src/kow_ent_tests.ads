------------------------------------------------------------------------------
--                                                                          --
--                       KOW Framework :: Entities                          --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--               Copyright (C) 2007-2011, KOW Framework Project             --
--                                                                          --
--                                                                          --
-- KOWLib is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. KOWLib is distributed in the hope that it will be useful, but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License distributed with KOWLib; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Main property types for KOW Ent                                          --
------------------------------------------------------------------------------



-----------
-- Ahven --
-----------
with Ahven.Framework;


-------------
-- KOW Ent --
-------------



package KOW_Ent_Tests is



	--------------------
	-- Initialization --
	--------------------

	type Test is new Ahven.Framework.Test_Case with null record;

	overriding
	procedure Initialize( T : in out Test );



	--------------------
	-- Controll Tests --
	--------------------

	Default_Stress_Tests_Iterations : constant Positive := 10_000;
	Stress_Tests_iterations_Variable: constant String := "STRESS_TESTS_ITERATIONS";
	function Stress_Tests_Iterations return Positive;
	-- check if the STRESS_TESTS_ITERATIONS environment variable is set.
	-- if not, use the Default_Stress_Tests_Iterations value
	procedure Stress_Control;



	--------------------
	-- Property Tests --
	--------------------

	procedure Properties_Test;
	-- test the core funcionality of properties -- how they hold value and the sorts
	procedure Properties_Stress_Test;



	-----------------
	-- Query Tests --
	-----------------

	procedure Query_Test;

end KOW_Ent_Tests;
