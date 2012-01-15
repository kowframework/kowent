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




package KOW_Ent.Queries.Logic_Relations is


	---------------------
	-- Stored vs Value --
	---------------------

	type Stored_Vs_Value_Operation is new Logic_Relation_Type with record
		Property_Name	: Property_Name_Type;
		-- it's a pointer but always share the access to the same string value
		Value		: Value_Ptr;
		Relation	: Relational_Operator_Type := Relation_Equal_To;
		Operator	: Logic_Operator_Type := Operator_AND;
	end record;
	type Stored_Vs_Value_Ptr is access all Stored_Vs_Value_Operation;


	overriding
	procedure Adjust( Operation : in out Stored_Vs_Value_Operation );

	overriding
	procedure Finalize( Operation : in out Stored_Vs_Value_Operation );


	overriding
	procedure Free(
				Operation	: in     Stored_Vs_Value_Operation;
				Name		: in out Logic_Relation_Ptr
			);
	
	overriding
	function Clone(
				Operation	: in    Stored_Vs_Value_Operation 
			) return Logic_Relation_Ptr;


	----------------------
	-- Stored vs Stored --
	----------------------

	type Stored_Vs_Stored_Operation is new Logic_Relation_Type with record
		Left_Property_Name	: Property_Name_Type;
		Right_Property_Name	: Property_Name_Type;
		Right_Property_Container_Tag : Ada.Tags.Tag;

		Relation		: Relational_Operator_Type := Relation_Equal_To;
		Operator		: Logic_Operator_Type := Operator_AND;
	end record;
	type Stored_Vs_Stored_Ptr is access all Stored_Vs_Stored_Operation;


	overriding
	procedure Free(
				Operation	: in     Stored_Vs_Stored_Operation;
				Name		: in out Logic_Relation_Ptr
			);
	
	overriding
	function Clone(
				Operation	: in     Stored_Vs_Stored_Operation
			) return Logic_Relation_Ptr;
	
	--------------------
	-- Logic_Criteria --
	--------------------

	type Logic_Criteria_Operation is new Logic_Relation_Type with record
		Logic_Criteria	: Logic_Criteria_Type;
		Operator	: Logic_Operator_Type	:= Operator_AND;
	end record;
	type Logic_Criteria_Ptr is access all Logic_Criteria_Operation;


	overriding
	procedure Free(
				Operation	: in     Logic_Criteria_Operation;
				Name		: in out Logic_Relation_Ptr
			);
	
	overriding
	function Clone(
				Operation	: in     Logic_Criteria_Operation
			) return Logic_Relation_Ptr;

end KOW_Ent.Queries.Logic_Relations;
