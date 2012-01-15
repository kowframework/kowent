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
-- Package for creating SQL queries in KOW Ent                              --
------------------------------------------------------------------------------




--------------
-- Ada 2005 --
--------------
with Ada.Tags;

-------------------
-- KOW Framework --
-------------------
with APQ;
with KOW_Ent.Data_Storages;
with KOW_Ent.Queries;				use KOW_Ent.Queries;
with KOW_Ent.Queries.Logic_Relations;



package KOW_Ent.SQL is



	procedure Append_Value(
			Value		: in     Value_Type;
			Connection	: in     APQ.Root_Connection_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		);


	type Generator_Type is tagged private;



	------------------
	-- Main Methods --
	------------------

	procedure Generate_Select(
				Generator	: in out Generator_Type;
				Query		: in     KOW_Ent.Queries.Query_Type;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			);


	function Get_Table_Name(
				Generator	: in     Generator_Type;
				Entity_Tag	: in     Ada.Tags.Tag
			) return String;
	-- get the name for the table


	--procedure Generate_Join(
	--			Generator	: in out Generator_Type;
	--			Query		: in     KOW_Ent.Queries.Joined_Query_Type;
	--			Connection	: in     APQ.Root_Connection_Type'Class;
	--			Q		: in out APQ.Root_Query_Type'Class
	--		);
	
	
	-----------------------------
	-- Select Query Generation --
	-----------------------------


	procedure Append_Table_Name(
				Generator	: in out Generator_Type;
				Query		: in     KOW_Ent.Queries.Query_Type;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			);
	

	procedure Append_Column_Name(
				Generator	: in out Generator_Type;
				Property	: in     Property_Type'Class;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			);

	
	procedure Append_Logic_Relation(
				Generator	: in out Generator_Type;
				Operation	: in     Logic_Relation_Type'Class;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			);
	
	procedure Append_Logic_Criteria(
				Generator	: in out Generator_Type;
				Criteria	: in     Logic_Criteria_Type'Class;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			);



	procedure Append_Logic_Operator(
				Generator	: in out Generator_Type;
				Operator	: in     Logic_Operator_Type;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			);

	
	procedure Append_Relational_Operator(
				Generator	: in out Generator_Type;
				Relation	: in     Relational_Operator_Type;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			);




	procedure Append_Operation_Stored_Vs_Value(
				Generator	: in out Generator_Type;
				Operation	: in     Logic_Relations.Stored_Vs_Value_Operation'Class;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			);

	procedure Append_Operation_Stored_Vs_Stored(
				Generator	: in out Generator_Type;
				Operation	: in     Logic_Relations.Stored_Vs_Stored_Operation'Class;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			);

	procedure Append_Operation_Logic_Criteria(
				Generator	: in out Generator_Type;
				Operation	: in     Logic_Relations.Logic_Criteria_Operation'Class;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			);



	-- Append_Select
	-- Append_Table_Name
	-- Append_


private

	type Table_Specification_Type is record
		Table_Name	: String( 1 .. 2**8 );
		Table_Alias	: String( 1 .. 2**8 );
	end record;


	package Table_Specification_Lists is new Ada.Containers.Doubly_Linked_Lists( Table_Specification_Type );


	type Generator_Type is tagged record
		Tables_To_Select	: Table_Specification_Lists.List;
		Current_Table		: Natural := 0;


		Is_First_Logic_Relation	: Boolean := True;
	end record;

end KOW_Ent.SQL;
