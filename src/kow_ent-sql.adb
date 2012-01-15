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
-- Package for creating SQL queries in KOW Ent                              --
------------------------------------------------------------------------------





-------------------
-- KOW Framework --
-------------------
with APQ;
with KOW_Ent.Data_Storages;
with KOW_Ent.Queries;				use KOW_Ent.Queries;
with KOW_Ent.Queries.Logic_Relations;



package body KOW_Ent.SQL is


	------------------
	-- Main Methods --
	------------------

	procedure Generate_Select(
				Generator	: in out Generator_Type;
				Query		: in     KOW_Ent.Queries.Query_Type;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			) is
	begin
		APQ.Append( Q, "SELECT " );
		
		Append_Column_Names( Generator, Query, Connection, Q );

		APQ.Append( Q, " FROM " );

		Append_Table_Name( Generator, Query, Connection, Q );

		
		if not Is_Empty( Query.Logic_Criteria ) then
			APQ.Append( Q, " WHERE " );
			Append_Logic_Criteria( Generator, Query.Logic_Criteria, Connection, Q );
		end if;
	end Generate_Select;


	function Get_Table_Name(
				Generator	: in     Generator_Type;
				Entity_Tag	: in     Ada.Tags.Tag
			) return String is
		-- get the name for the table
		use Data_Storages;
	begin
		return Trim( Get_Alias( Get_Data_Storage( Entity_Tag ).all, Entity_Tag ) );
	end Get_Table_Name;

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
			) is
	begin
		APQ.Append( Q, Get_Table_Name( Generator, Query.Entiy_Tag ) );
	end Append_Table_Name;
	

	procedure Append_Column_Name(
				Generator	: in out Generator_Type;
				Property	: in     Property_Type'Class;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			) is
	begin
		APQ.Append( Q, Property.Name.all );
	end Append_Column_Name;
	
	procedure Append_Logic_Relation(
				Generator	: in out Generator_Type;
				Operation	: in     Logic_Relation_Type'Class;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			) is


	begin
		if Operation in Logic_Relations.Stored_Vs_Value_Operation'Class then
			Append_Operation_Stored_Vs_Value(
								Generator	=> Generator,
								Operation	=> Logic_Relations.Stored_Vs_Value_Operation'Class( Operation ),
								Connection	=> Connection,
								Q		=> Q
							);
		elsif Operation in Logic_Relations.Stored_Vs_Stored_Operation'Class then
			Append_Operation_Stored_Vs_Stored(
								Generator	=> Generator,
								Operation	=> Logic_Relations.Stored_Vs_Stored_Operation'Class( Operation ),
								Connection	=> Connection,
								Q		=> Q
							);
		elsif Operation in Logic_Relations.Logic_Criteria_Operation'Class then
			Append_Operation_Logic_Criteria(
								Generator	=> Generator,
								Operation	=> Logic_Relations.Logic_Criteria_Operation'Class( Operation ),
								Connection	=> Connection,
								Q		=> Q
							);
		else
			raise Program_Error with "I don't know how to append the given type :: " & Ada.Tags.Expanded_Name( Operation'Tag );
		end if;
	end Append_Logic_Relation;
	


	procedure Append_Logic_Criteria(
				Generator	: in out Generator_Type;
				Criteria	: in     Logic_Criteria_Type'Class;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			) is
	
		procedure Iterator( Operation : Logic_Relation_Type'Class ) is
		begin
			Append_Logic_Relation( Generator, Operation, Connection, Q );
		end Iterator;
	begin
		Queries.Iterate( Criteria, Iterator'Access );
	end Append_Logic_Criteria;




	-------------------------
	-- Internal procedures --
	-------------------------



	procedure Append_Operation_Stored_Vs_Value(
				Generator	: in out Generator_Type;
				Operation	: in     Logic_Relations.Stored_Vs_Value_Operation'Class;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			) is
	begin
		Append_Operation
	end Append_Operation_Stored_Vs_Value;

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


	type Generator_Type is tagged private
		Tables_To_Select	: Table_Specification_Lists.List;
		Current_Table		: Natural := 0;
	end record;

end KOW_Ent.SQL;
