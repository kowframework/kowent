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
	
	procedure Load_Value(
			Value		: in out Value_Type;
			Connection	: in     APQ.Root_Connection_Type'Class;
			Q		: in     APQ.Root_Query_Type'Class;
			Column_Name	: in     String
		);



	-------------------------------
	-- The Insert Generator Type --
	-------------------------------

	type Insert_Generator_Type is tagged private;


	procedure Generate_Insert(
				Generator	: in out Insert_Generator_Type;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class;
				Entity		: in out KOW_Ent.Entity_Type'Class;
				Id_Property	:    out Property_Ptr
			);
	-- if the entity has one ID property, sets the Id_Property
	-- otherwise, it's null

	-------------------------------
	-- The Select Generator Type --
	-------------------------------


	type Select_Generator_Type is tagged private;



	------------------
	-- Main Methods --
	------------------

	procedure Generate_Select(
				Generator	: in out Select_Generator_Type;
				Query		: in     KOW_Ent.Queries.Query_Type'Class;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			);
	-- generate the select


	function Get_Table_Name(
				Generator	: in     Select_Generator_Type
			) return String;
	-- get the name for the current table (trimmed, of course)

	procedure Append_Table_To_Select(
				Generator	: in out Select_Generator_Type;
				Entity_Tag	: in     Ada.Tags.Tag;
				Alias		:    out Entity_Alias_Type
			);
	-- append a entity to select, returning it's alias
	-- if entity_Tag = no_tag return the first element in tables_to_select (ie, the current table)
	-- if the given table is already in select list, don't append it twice (only return the alias)

	--procedure Generate_Join(
	--			Generator	: in out Select_Generator_Type;
	--			Query		: in     KOW_Ent.Queries.Joined_Query_Type;
	--			Connection	: in     APQ.Root_Connection_Type'Class;
	--			Q		: in out APQ.Root_Query_Type'Class
	--		);
	
	
	-----------------------------
	-- Select Query Generation --
	-----------------------------

	-- the SELECT query structure in KOW_Ent language:
	--
	-- SELECT
	-- 	[COLUMN NAMES]
	-- FROM
	-- 	[TABLE_NAMES]
	-- WHERE
	-- 	[LOGIC_CRITERIA]
	--
	-- [LIMIT_AND_OFFSET]


	--
	-- Macro
	--
	procedure Append_Column_Names(
				Generator	: in out Select_Generator_Type;
				Query		: in     KOW_Ent.Queries.Query_Type'Class;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class;
				Template	: in out KOW_Ent.Entity_Type'Class
			);
	-- append all the columns name using the following template:
	--	table_name.column_name as table_name_column_name

	procedure Append_Table_Names(
				Generator	: in out Select_Generator_Type;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			);
	-- append all the tables that should be queried
	

	procedure Append_Logic_Criteria(
				Generator	: in out Select_Generator_Type;
				Criteria	: in     Logic_Criteria_Type'Class;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			);
	-- append the logic criteria

	
	procedure Append_Limit_And_Offset(
				Generator	: in out Select_Generator_Type;
				Query		: in     KOW_Ent.Queries.Query_Type'Class;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			);
	-- append the limit and offset values when needed
	

	-- 
	-- Micro
	--


	procedure Append_Column_Name(
				Generator	: in out Select_Generator_Type;
				Entity_Tag	: in     Ada.Tags.Tag;
				Property_Name	: in     Property_Name_Type;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			);
	-- append the column name.;
	-- if entity_tag = no_tag, get the current query's entity

	


	procedure Append_Logic_Operator(
				Generator	: in out Select_Generator_Type;
				Operator	: in     Logic_Operator_Type;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			);
	-- append AND or OR accordingly

	
	procedure Append_Relational_Operator(
				Generator	: in out Select_Generator_Type;
				Relation	: in     Relational_Operator_Type;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			);
	-- append the operator, such as =, !=, <, LIKE, ILIKE, ...



	--
	-- Logic Relations
	--

	procedure Append_Logic_Relation(
				Generator	: in out Select_Generator_Type;
				Operation	: in     Logic_Relation_Type'Class;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			);

	-- Specific logic relations...


	procedure Append_Operation_Stored_Vs_Value(
				Generator	: in out Select_Generator_Type;
				Operation	: in     Logic_Relations.Stored_Vs_Value_Operation'Class;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			);

	procedure Append_Operation_Stored_Vs_Stored(
				Generator	: in out Select_Generator_Type;
				Operation	: in     Logic_Relations.Stored_Vs_Stored_Operation'Class;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			);

	procedure Append_Operation_Logic_Criteria(
				Generator	: in out Select_Generator_Type;
				Operation	: in     Logic_Relations.Logic_Criteria_Operation'Class;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			);


	procedure Append_Join(
					Generator	: in out Select_Generator_Type;
					Description	: in     Join_Description_Type;
					Connection	: in     APQ.Root_Connection_Type'Class;
					Query		: in out APQ.Root_Query_Type'Class
				);
	-- append the given join expression



	-------------------------------
	-- The Update_Generator Type --
	-------------------------------

	type Update_Generator_Type is new Select_Generator_Type with private;


	procedure Generate_Update(
				Generator	: in out Update_Generator_Type;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class;
				Entity		: in out KOW_Ent.Entity_Type'Class;
				Criteria	: in     KOW_Ent.Queries.Logic_Criteria_Type
			);




private

	package Table_Alias_lists is new Ada.Containers.Doubly_Linked_Lists( Entity_Alias_Type );


	-------------------------------
	-- The Insert Generator Type --
	-------------------------------

	type Insert_Generator_Type is tagged null record;


	-------------------------------
	-- The Select Generator Type --
	-------------------------------


	type Select_Generator_Type is tagged record
		Tables_To_Select	: Table_Alias_Lists.List;
		-- the current table for a given query is aways the first one

		Is_First_Logic_Relation	: Boolean := True;
	end record;


	-------------------------------
	-- The Update_Generator Type --
	-------------------------------

	type Update_Generator_Type is new Select_Generator_Type with null record;

end KOW_Ent.SQL;
