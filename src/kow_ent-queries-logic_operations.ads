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
-- Contains methods for representing a logic operations in a query          --
------------------------------------------------------------------------------




package KOW_Ent.Queries.Logic_Operations is


	------------------
	-- Scalar Types --
	------------------

	type Logic_Appender_Type is (
			Appender_AND,
			Appender_OR
		);

	type Logic_Operator_Type is (
		-- all operators take into account:
		-- 	Left-hand side of operation	=> Value inside the data storage
		-- 	Right-hand side of operation	=> Parameter passed into the append procedure
			Operator_Equal_To,
			Operator_Not_Equal_To,
			Operator_Like,
			Operator_Less_Than,
			Operator_Less_Than_Or_Equal_To,
			Operator_Greater_Than,
			Operator_Greater_Than_Or_Equal_To
		);
	

	type Join_Type is (
			Left_Join,
			Right_Join,
			Inner_Join
		);
	


	---------------------
	-- Logic Operation --
	---------------------



	type Logic_Operation_Type is abstract new Ada.Finalization.Controlled with null record;
	type Logic_Operation_Ptr is access all Logic_Operation_Type'Class;

	

	procedure Free(
				Operation	: in     Logic_Operation_Type;
				Name		: in out Logic_Operation_Ptr
			);
	-- frees the pointer of the given type


	package Logic_Operation_Lists is new Ada.Containers.Doubly_Linked_Lists(
								Element_Type => Logic_Operation_Type
							);


	type Stored_Vs_Value_Logic_Operation is new Logic_Operation_Type with record
		Property_Name	: Property_Name_Type;
		-- it's a pointer but always share the access to the same string value
		Value		: Value_Ptr;
		Operator	: Logic_Operator_Type;
		Appender	: Loginc_Appender_Type := Appender_AND;
	end record;


	overriding
	procedure Free(
				Operation	: in     Stored_Vs_Value_Logic_Operation;
				Name		: in out Logic_Operation_Ptr
			);

	overriding
	procedure Adjust( Operation : in out Logic_Operation_Query_Element );

	overriding
	procedure Finalize( Operation : in out Logic_Operation_Query_element );


	function New_Store_Vs_Value(
				Property_Name	: in Property_Name_Type;
				Value		: in Value_Type;
				Operator	: in Logic_Operator_Type := Operator_Equal_To;
				Appender	: in Logic_Appender_Type := Operator_AND
			) return Logic_Operation_Ptr;



end KOW_Ent.Queries.Logic_Operations;
