------------------------------------------------------------------------------
--                                                                          --
--                       KOW Framework :: Entities                          --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 B o d y                                  --
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



package body KOW_Ent.Properties is

	function Get_Type ( Property : in Valued_Property_Type ) return Type_Of_Data_Type is
		-- return the type of the value; used for calling set_valued in the right way
	begin
		return Property.Type_Of;
	end Get_Type;


	function Get_Value( Property : in Valued_Property_Type ) return Value_Type is
		-- get the value
	begin
		return Property.Value;
	end Get_Value;


	procedure Set_Value(
				Property	: in out Valued_Property_Type;
				Value		: in     Value_Type
			) is
	begin
		Pragma Assert( Property.Type_Of = Value.Type_Of, "Trying to set value for unmatched type" );
		Property.Value := Value;
	end Set_Value;


	function To_String( Property : in Valued_Property_Type ) return String is
	begin
		return To_String( Property.Value );
	end To_String;

	procedure From_String( Property : in out Valued_Property_Type; String_Value : in String ) is
	begin
		From_String( Property.Value, String_Value );
	end From_String;



	---------------------
	-- The Id Property --
	---------------------

	overriding
	function Ignore_For_Insert( Property : in Id_Property ) return Boolean is
	begin
		return True;
	end Ignore_For_Insert;


	overriding
	function Ignore_For_Update( Property : in Id_Property ) return Boolean is
	begin
		return True;
	end Ignore_For_Update;

end KOW_Ent.Properties;
