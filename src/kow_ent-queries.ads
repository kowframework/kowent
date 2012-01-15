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
-- Contains methods for representing a query into the data storage          --
-- The query should support:                                                --
--      1. multiple logical operations with multiple levels:                --
--             a < b && ( a = b || c <= d)                                  --
--      2. Joins (left/right/inner)                                         --
------------------------------------------------------------------------------




--------------
-- Ada 2005 --
--------------
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Finalization;
with Ada.Tags;

package KOW_Ent.Queries is

	------------------
	-- Scalar Types --
	------------------

	type Logic_Operator_Type is (
			Operator_AND,
			Operator_OR
		);

	type Relational_Operator_Type is (
		-- all operators take into account:
		-- 	Left-hand side of operation	=> Value inside the data storage
		-- 	Right-hand side of operation	=> Parameter passed into the append procedure
			Relation_Equal_To,
			Relation_Not_Equal_To,
			Relation_Like,
			Relation_Less_Than,
			Relation_Less_Than_Or_Equal_To,
			Relation_Greater_Than,
			Relation_Greater_Than_Or_Equal_To
		);
	

	type Join_Type is (
			Left_Join,
			Right_Join,
			Inner_Join
		);
	


	---------------------
	-- Logic Operation --
	---------------------



	type Logic_Relation_Type is abstract new Ada.Finalization.Controlled with null record;
	type Logic_Relation_Ptr is access all Logic_Relation_Type'Class;

	

	procedure Free(
				Operation	: in     Logic_Relation_Type;
				Name		: in out Logic_Relation_Ptr
			) is abstract;
	-- frees the pointer of the given type

	function Clone(
				Operation	: in     Logic_Relation_Type
			) return Logic_Relation_Ptr is abstract;

	package Logic_Relation_Lists is new Ada.Containers.Doubly_Linked_Lists(
								Element_Type => Logic_Relation_Ptr
							);

	-------------------------
	-- Logic Criteria Type --
	-------------------------
	
	type Logic_Criteria_Type is new Ada.Finalization.Controlled with private;

	overriding
	procedure Adjust( Logic_Criteria : in out Logic_Criteria_Type );
	-- reallocate all the operations

	overriding
	procedure Finalize( Logic_Criteria : in out Logic_Criteria_Type );
	-- free the operations



	procedure Append(
				Criteria	: in out Logic_Criteria_Type;
				Operation	: in     Logic_Relation_Type'Class
			);
	
	procedure Iterate(
				Criteria	: in     Logic_Criteria_Type;
				Iterator	: access procedure( Operation : in Logic_Relation_Type'Class )
			);

	function Is_Empty( Criteria : in Logic_Criteria_Type ) return Boolean;
	-- check if there is any operation appended into the criteria


	----------------------------
	-- Finally The Query Type --
	----------------------------

	type Query_Type is record
		Entity_Tag	: Ada.Tags.Tag;
		Logic_Criteria	: Logic_Criteria_Type;
	end record;

	-----------------------------------
	-- And now the Joined Query Type --
	-----------------------------------

	type Joined_Query_Type is record
		Left_Query	: Query_Type;
		Right_Query	: Query_Type;
		Join		: Join_Type;
		Having		: Logic_Criteria_Type;
	end record;


private

	-------------------------
	-- Logic Criteria Type --
	-------------------------

	type Logic_Criteria_Type is new Ada.Finalization.Controlled with record
		Operations : Logic_Relation_lists.List;
	end record;

end KOW_Ent.Queries;
