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



--------------
-- Ada 2005 --
--------------
with Ada.Strings;
with Ada.Strings.Fixed;

-------------------
-- KOW Framework --
-------------------
with APQ;
with KOW_Ent.Data_Storages;
with KOW_Ent.Queries;				use KOW_Ent.Queries;
with KOW_Ent.Queries.Logic_Relations;
with KOW_Lib.String_Util;



package body KOW_Ent.SQL is


	procedure Append_Value(
			Value		: in     Value_Type;
			Connection	: in     APQ.Root_Connection_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		) is


		procedure Append_Smallint is new APQ.Append_Integer( APQ.APQ_Smallint );
		procedure Append_Integer is new APQ.Append_Integer( APQ.APQ_Integer );
		procedure Append_Bigint is new APQ.Append_Integer( APQ.APQ_Bigint );

		procedure Append_Real is new APQ.Append_Float( APQ.APQ_Real );
		procedure Append_Double is new APQ.Append_Float( APQ.APQ_Double );

		procedure Append_Serial is new APQ.Append_Integer( APQ.APQ_Serial );
		procedure Append_Bigserial is new APQ.Append_Integer( APQ.APQ_Bigserial );

		procedure Append_Hour is new APQ.Append_Integer( APQ.Hour_Number );
		procedure Append_Minute is new APQ.Append_Integer( APQ.Minute_Number );
		procedure Append_Second is new APQ.Append_Integer( APQ.Second_Number );

	begin
		case Value.Type_of is
			when APQ_Smallint =>
				Append_Smallint( Q, Value.Smallint_Value );

			when APQ_Integer =>
				Append_Integer( Q, Value.Integer_Value );

			when APQ_Bigint =>
				Append_Bigint( Q, Value.Bigint_Value );



			when APQ_Real	=>
				Append_Real( Q, Value.Real_Value );

			when APQ_Double =>
				Append_Double( Q, Value.Double_Value );



			when APQ_Serial	=>
				Append_Serial( Q, Value.Serial_Value );

			when APQ_Bigserial =>
				Append_Bigserial( Q, Value.Bigserial_Value );



			when APQ_Date =>
				APQ.Append( Q, Value.Date_Value );

			when APQ_Time =>
				APQ.Append( Q, Value.Time_Value );

			when APQ_Timestamp =>
				APQ.Append( Q, Value.TImestamp_Value );



			when Hour_Number =>
				Append_Hour( Q, Value.Hour_Value );
			
			when Minute_Number =>
				Append_Minute( Q, Value.Minute_Value );

			when Second_Number =>
				Append_Second( Q, Value.Second_Value );

			when APQ_String =>
				APQ.Append_Quoted( Q, Connection, Ada.Strings.Fixed.Trim( Value.String_Value, Ada.Strings.Right ) );
		end case;

	end Append_Value;




	procedure Load_Value(
			Value		: in out Value_Type;
			Connection	: in     APQ.Root_Connection_Type'Class;
			Q		: in     APQ.Root_Query_Type'Class;
			Column_Name	: in     String
		) is


		function Smallint_Value is new APQ.Integer_Value( APQ.APQ_Smallint );
		function Integer_Value is new APQ.Integer_Value( APQ.APQ_Integer );
		function Bigint_Value is new APQ.Integer_Value( APQ.APQ_Bigint );

		function Real_Value is new APQ.Float_Value( APQ.APQ_Real );
		function Double_Value is new APQ.Float_Value( APQ.APQ_Double );

		function Serial_Value is new APQ.Integer_Value( APQ.APQ_Serial );
		function Bigserial_Value is new APQ.Integer_Value( APQ.APQ_Bigserial );


		function Date_Value is new APQ.Date_Value( APQ.APQ_Date );
		function Time_Value is new APQ.Time_Value( APQ.APQ_Time );
		function Timestamp_Value is new APQ.Timestamp_Value( APQ.APQ_Timestamp );

		function Hour_Value is new APQ.Integer_Value( APQ.Hour_Number );
		function Minute_Value is new APQ.Integer_Value( APQ.Minute_Number );
		function Second_Value is new APQ.Integer_Value( APQ.Second_Number );


		Idx : APQ.Column_Index_Type := APQ.Column_Index( Q, Column_Name );

	begin
		
		case Value.Type_of is
			when APQ_Smallint =>
				Value.Smallint_Value := Smallint_Value( Q, Idx );

			when APQ_Integer =>
				Value.Integer_Value := Integer_Value( Q, Idx );

			when APQ_Bigint =>
				Value.Bigint_Value := Bigint_Value( Q, Idx );



			when APQ_Real	=>
				Value.Real_Value := Real_Value( Q, Idx );

			when APQ_Double =>
				Value.Double_Value := Double_Value( Q, Idx );



			when APQ_Serial	=>
				Value.Serial_Value := Serial_Value( Q, Idx );

			when APQ_Bigserial =>
				Value.Bigserial_Value := Bigserial_Value( Q, Idx );



			when APQ_Date =>
				Value.Date_Value := Date_Value( Q, Idx );

			when APQ_Time =>
				Value.Time_Value := Time_Value( Q, Idx );

			when APQ_Timestamp =>
				Value.Timestamp_Value := Timestamp_Value( Q, Idx );



			when Hour_Number =>
				Value.Hour_Value := Hour_Value( Q, Idx );
			
			when Minute_Number =>
				Value.Minute_Value := Minute_Value( Q, Idx );

			when Second_Number =>
				Value.Second_Value := Second_Value( Q, Idx );

			when APQ_String =>
				declare
					Val : constant String := APQ.Value( Q, Idx );
				begin
					KOW_Lib.String_Util.Copy( From => Val, To => Value.String_Value );
				end;
		end case;

	end Load_Value;






	-------------------------------
	-- The Insert Generator Type --
	-------------------------------

	procedure Generate_Insert(
				Generator	: in out Insert_Generator_Type;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class;
				Entity		: in out KOW_Ent.Entity_Type'Class
			) is

		Is_First_Name, Is_First_Value : Boolean := True;

		procedure Name_Iterator( Property : in Property_Ptr ) is
		begin
			if Ignore_For_Insert( Property.all ) then
				return;
			end if;

			if Is_First_Name then
				Is_First_Name := False;
			else
				APQ.Append( Q, "," );
			end if;

			APQ.Append( Q, Property.Name.all );
		end Name_Iterator;


		procedure Value_Iterator( Property : in Property_Ptr ) is
		begin
			if Ignore_For_Insert( Property.all ) then
				return;
			end if;


			if Is_First_Value then
				Is_First_Value := False;
			else
				APQ.Append( Q, "," );
			end if;

			Append_Value( 
					Value		=> Get_Value( Property.all ),
					Connection	=> Connection,
					Q		=> Q
				);
		end Value_Iterator;


		Table_Name : constant String := Trim( Get_Alias( Entity ) );
	begin

		APQ.Append( Q, "INSERT INTO " & Table_Name & "(" );
			Iterate( Entity, Name_Iterator'Access );
		APQ.Append( Q, ") VALUES(" );
			Iterate( Entity, Value_Iterator'Access );
		APQ.Append( Q, ")" );
	end Generate_Insert;

	-------------------------------
	-- The Select Generator Type --
	-------------------------------



	------------------
	-- Main Methods --
	------------------

	procedure Generate_Select(
				Generator	: in out Select_Generator_Type;
				Query		: in     KOW_Ent.Queries.Query_Type;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			) is
		Alias : Entity_Alias_Type;




		function Get_Where return String is
			Tmp_Q : APQ.Root_Query_Type'Class := APQ.New_Query( Connection );
		begin
			if not Is_Empty( Query.Logic_Criteria ) then
				Append_Logic_Criteria( Generator, Query.Logic_Criteria, Connection, Tmp_Q );
				return APQ.To_String( Tmp_Q );
			else
				return "";
			end if;
		end Get_Where;


		Where : constant String := Get_Where;
	begin
		Append_Table_To_Select( Generator, Query.Entity_Tag, Alias );
		-- if the entity_tag is no_tag will raise cosntraint_error


		-- we need to run this first so I know what tables to select




		APQ.Append( Q, "SELECT " );
		
		Append_Column_Names( Generator, Query, Connection, Q );

		APQ.Append( Q, " FROM " );

		Append_Table_Names( Generator, Connection, Q );


		-- and now we append the "WHERE" part
		if where /= "" then
			APQ.Append( Q, " WHERE " & Where);
		end if;

		
		
		Append_Limit_And_Offset(
				Generator	=> Generator,
				Query		=> Query,
				Connection	=> Connection,
				Q		=> Q
			);
	end Generate_Select;


	function Get_Table_Name(
				Generator	: in     Select_Generator_Type
			) return String is
		-- get the name for the current table (trimmed, of course)
	begin
		return Trim( Table_Alias_Lists.First_Element( Generator.Tables_To_Select ) );
	end Get_Table_Name;



	procedure Append_Table_To_Select(
				Generator	: in out Select_Generator_Type;
				Entity_Tag	: in     Ada.Tags.Tag;
				Alias		:    out Entity_Alias_Type
			) is
		-- append a entity to select, returning it's alias
		-- if entity_Tag = no_tag return the first element in tables_to_select (ie, the current table)
		-- if the given table is already in select list, don't append it twice (only return the alias)


		use Ada.Tags;
		use Table_Alias_Lists;
	begin

		if Entity_Tag = No_Tag then
			Alias := First_Element( Generator.Tables_To_Select );
		else
			declare
				use Data_Storages;
				The_Alias : Entity_Alias_Type := Get_Alias( Data_Storage_Type'Class( Get_Data_Storage( Entity_Tag ).all ), Entity_Tag );
			begin
				if not Contains( Generator.Tables_To_Select, The_Alias ) then
					Append( Generator.Tables_To_Select, The_Alias );
				end if;
				Alias := The_Alias;
			end;
		end if;
	end Append_Table_To_Select;


	--procedure Generate_Join(
	--			Generator	: in out Select_Generator_Type;
	--			Query		: in     KOW_Ent.Queries.Joined_Query_Type;
	--			Connection	: in     APQ.Root_Connection_Type'Class;
	--			Q		: in out APQ.Root_Query_Type'Class
	--		);
	
	
	-----------------------------
	-- Select Query Generation --
	-----------------------------


	--
	-- Macro
	--



	procedure Append_Column_Names(
			Generator	: in out Select_Generator_Type;
			Query		: in     KOW_Ent.Queries.Query_Type;
			Connection	: in     APQ.Root_Connection_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
			) is
		-- append all the columns name
		use KOW_Ent.Data_Storages;
		Table_Name	: constant String := Get_Table_Name( Generator );
		Template	: KOW_Ent.Entity_Type'Class := Create(
									Data_Storage_Type'Class( Get_Data_Storage( Query.Entity_Tag ).all ),
									Query.Entity_Tag
								);
		Is_First : Boolean := True;

		procedure Iterator( P : in Property_Ptr ) is
		begin
			if Is_First then
				Is_First := False;
			else
				APQ.Append( Q, "," );
			end if;
			APQ.Append( Q, Table_Name & '.' & P.Name.all );
		end Iterator;
	begin
		KOW_Ent.Iterate( 
				Container	=> Template, 
				Iterator	=> Iterator'Access
			);
	end Append_Column_Names;
	
	procedure Append_Table_Names(
				Generator	: in out Select_Generator_Type;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			) is
		First : Boolean := True;

		use Table_Alias_Lists;
		procedure Iterator( C : Cursor ) is
		begin
			if First then
				First := False;
			else
				APQ.Append( Q, "," );
			end if;
			APQ.Append( Q, Trim( Element( c ) ) );
		end Iterator;
	begin
		Iterate( Generator.Tables_To_Select, Iterator'Access );
	end Append_Table_Names;




	procedure Append_Logic_Criteria(
				Generator	: in out Select_Generator_Type;
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



	
	procedure Append_Limit_And_Offset(
				Generator	: in out Select_Generator_Type;
				Query		: in     KOW_Ent.Queries.Query_Type;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			) is
		-- append the limit and offset values when needed
		-- the following syntax is valid for both MySQL and PostgreSQL; check if it's also valid for other vendors
	begin
		if Query.Limit /= 0 then
			APQ.Append( Q, " LIMIT " & Natural'Image( Query.Limit ) );
		end if;
		if Query.Offset /= 1 then
			APQ.Append( Q, " OFFSET " & Positive'Image( Query.Offset ) );
		end if;
	end Append_Limit_And_Offset;



	--
	-- Micro
	--
	

	procedure Append_Column_Name(
				Generator	: in out Select_Generator_Type;
				Entity_Tag	: in     Ada.Tags.Tag;
				Property_Name	: in     Property_Name_Type;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			) is

		Alias : Entity_Alias_Type;
	begin
		Append_Table_To_Select( Generator, Entity_Tag, Alias );
		APQ.Append( Q, Trim( Alias ) & '.' & Property_Name.all );
	end Append_Column_Name;


	
	



	procedure Append_Logic_Operator(
				Generator	: in out Select_Generator_Type;
				Operator	: in     Logic_Operator_Type;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			) is
	begin
		if Generator.Is_First_Logic_Relation then
			Generator.Is_First_Logic_Relation := False;
		else
			case Operator is
				when Operator_And =>
					APQ.Append( Q, " AND " );

				when Operator_Or =>
					APQ.Append( Q, " OR " );
			end case;
		end if;
	end Append_Logic_Operator;


	procedure Append_Relational_Operator(
				Generator	: in out Select_Generator_Type;
				Relation	: in     Relational_Operator_Type;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			) is
	begin
		case Relation is
			when Relation_Equal_To =>
				APQ.Append( Q, "=" );

			when Relation_Not_Equal_To =>
				APQ.Append( Q, "!=" );

			when Relation_Like =>
				APQ.Append( Q, " LIKE " );

			when Relation_Less_Than =>
				APQ.Append( Q, "<" );

			when Relation_Less_Than_Or_Equal_To =>
				APQ.Append( Q, "<=" );

			when Relation_Greater_Than =>
				APQ.Append( Q, ">" );

			when Relation_Greater_Than_Or_Equal_To =>
				APQ.Append( Q, ">=" );
		end case;
	end Append_Relational_Operator;








	--
	-- Logic relations
	--


	procedure Append_Logic_Relation(
				Generator	: in out Select_Generator_Type;
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
	



	procedure Append_Operation_Stored_Vs_Value(
				Generator	: in out Select_Generator_Type;
				Operation	: in     Logic_Relations.Stored_Vs_Value_Operation'Class;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			) is
	begin
		Append_Logic_Operator(
				Generator	=> Select_Generator_Type'Class( Generator ),
				Operator	=> Operation.Operator,
				Connection	=> Connection,
				Q		=> Q
			);

		Append_Column_Name(
				Generator	=> Generator,
				Entity_Tag	=> Operation.Entity_Tag,
				Property_Name	=> Operation.Property_Name,
				Connection	=> Connection,
				Q		=> Q
			);

		Append_Relational_Operator(
					Generator	=> Select_Generator_Type'Class( Generator ),
					Relation	=> Operation.Relation,
					Connection	=> Connection,
					Q		=> Q
				);
		Append_Value( Operation.Value.all, Connection, Q );
	end Append_Operation_Stored_Vs_Value;

	procedure Append_Operation_Stored_Vs_Stored(
				Generator	: in out Select_Generator_Type;
				Operation	: in     Logic_Relations.Stored_Vs_Stored_Operation'Class;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			) is
	begin
		Append_Logic_Operator(
				Generator	=> Select_Generator_Type'Class( Generator ),
				Operator	=> Operation.Operator,
				Connection	=> Connection,
				Q		=> Q
			);

		Append_Column_Name(
				Generator	=> Generator,
				Entity_Tag	=> Operation.Left_Entity_Tag,
				Property_Name	=> Operation.Left_Property_Name,
				Connection	=> Connection,
				Q		=> Q
			);


		Append_Relational_Operator(
					Generator	=> Select_Generator_Type'Class( Generator ),
					Relation	=> Operation.Relation,
					Connection	=> Connection,
					Q		=> Q
				);
		Append_Column_Name(
				Generator	=> Generator,
				Entity_Tag	=> Operation.Right_Entity_Tag,
				Property_Name	=> Operation.Right_Property_Name,
				Connection	=> Connection,
				Q		=> Q
			);


	end Append_Operation_Stored_Vs_Stored;

	procedure Append_Operation_Logic_Criteria(
				Generator	: in out Select_Generator_Type;
				Operation	: in     Logic_Relations.Logic_Criteria_Operation'Class;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			) is
	begin
		if Is_Empty( Operation.Logic_Criteria ) then
			return;
		end if;
		Append_Logic_Operator(
				Generator	=> Select_Generator_Type'Class( Generator ),
				Operator	=> Operation.Operator,
				Connection	=> Connection,
				Q		=> Q
			);
		APQ.Append( Q, "(" );
		Append_Logic_Criteria(
					Generator	=> Select_Generator_Type'Class( Generator ),
					Criteria	=> Operation.Logic_Criteria,
					Connection	=> Connection,
					Q		=> Q
				);
		APQ.Append( Q, ")" );
	end Append_Operation_Logic_Criteria;


	-------------------------------
	-- The Update_Generator Type --
	-------------------------------


	procedure Generate_Update(
				Generator	: in out Update_Generator_Type;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class;
				Entity		: in out KOW_Ent.Entity_Type'Class;
				Criteria	: in     KOW_Ent.Queries.Logic_Criteria_Type
			) is

		Alias		: Entity_Alias_Type := Get_Alias( Entity );
		Table_Name	: constant String := Trim( Alias );

		Is_First : Boolean := False;


		procedure Iterator( P : in Property_Ptr ) is
		begin
			if Ignore_For_Update( P.all ) then
				return;
			end if;


			if Is_First then
				Is_First := False;
			else
				APQ.Append( Q, "," );
			end if;

			APQ.Append( Q, Table_Name & '.' & P.Name.all & "=" );
			Append_Value( Get_Value( P.all ), Connection, Q );
		end Iterator;


		function Get_Where return String is
			Tmp_Q : APQ.Root_Query_Type'Class := APQ.New_Query( Connection );
		begin
			if not Is_Empty( Criteria ) then
				Append_Logic_Criteria( Generator, Criteria, Connection, Tmp_Q );
				return APQ.To_String( Tmp_Q );
			else
				return "";
			end if;
		end Get_Where;

		Where : constant String := Get_Where;
	begin
		Append_Table_To_Select( Generator, Entity'Tag, Alias );
		
		APQ.Append( Q, "UPDATE " );
			Append_Table_Names( Generator, Connection, Q );

		APQ.Append( Q, " SET " );

		Iterate( Entity, Iterator'Access );

		if Where /= "" then
			APQ.Append( Q, " WHERE " & Where );
		end if;
	end Generate_Update;
end KOW_Ent.SQL;
