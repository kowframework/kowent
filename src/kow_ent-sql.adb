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



package body KOW_Ent.SQL is


	procedure Append_Value(
			Value		: in     Value_Type;
			Connection	: in     APQ.Root_Connection_Type'Class;
			Q		: in out APQ.Root_Query_Type'Class
		) is


		procedure Append_Smallint is new APQ.Append_Integer( APQ.APQ_Smallint );
		procedure Append_Integer is new APQ.Append_Integer( APQ.APQ_Integer );
		procedure Append_Bigint is new APQ.Append_Bigint( APQ.APQ_Bigint );

		procedure Append_Real is new APQ.Append_Fixed( APQ.APQ_Real );
		procedure Append_Double is new APQ.Append_Fixed( APQ.APQ_Double );

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
				APQ.Append_Quoted( Q, Connection, Ada.Strings.Fixed.Trim( Value.String_Value, Ada.Strings.Right );
		end case;

	end Append_Value;



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
	



	procedure Append_Logic_Operator(
				Generator	: in out Generator_Type;
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
				Generator	: in out Generator_Type;
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
		Append_Logic_Operator(
				Generator	=> Generator_Type'Class( Generator ),
				Operator	=> Operation.Operator,
				Connection	=> Connection,
				Q		=> Q
			);

		APQ.Append( Q, Operation.Property_Name.all );
		Append_Logic_Relation(
					Generator	=> Generator_Type'Class( Generator ),
					Relation	=> Operation.Relation,
					Connection	=> Connection,
					Q		=> Q
				);
		Append_Value( Value_Ptr.all, Connection, Q );
	end Append_Operation_Stored_Vs_Value;

	procedure Append_Operation_Stored_Vs_Stored(
				Generator	: in out Generator_Type;
				Operation	: in     Logic_Relations.Stored_Vs_Stored_Operation'Class;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			) is
	begin
		Append_Logic_Operator(
				Generator	=> Generator_Type'Class( Generator ),
				Operator	=> Operation.Operator,
				Connection	=> Connection,
				Q		=> Q
			);
		APQ.Append( Q, Operation.Left_Property_Name.all );
		Append_Logic_Relation(
					Generator	=> Generator_Type'Class( Generator ),
					Relation	=> Operation.Relation,
					Connection	=> Connection,
					Q		=> Q
				);
		-- TODO :: support the right property from another table!
		APQ.Append( Q, Operation.Right_Property_Name.all );

	end Append_Operation_Stored_Vs_Stored;

	procedure Append_Operation_Logic_Criteria(
				Generator	: in out Generator_Type;
				Operation	: in     Logic_Relations.Logic_Criteria_Operation'Class;
				Connection	: in     APQ.Root_Connection_Type'Class;
				Q		: in out APQ.Root_Query_Type'Class
			) is
	begin
		if Is_Empty( Operation.Criteria ) then
			return;
		end if;
		Append_Logic_Operator(
				Generator	=> Generator_Type'Class( Generator ),
				Operator	=> Operation.Operator,
				Connection	=> Connection,
				Q		=> Q
			);
		APQ.Append( Q, "(" );
		Append_Logic_Criteria(
					Generator	=> Generator_Type'Class( Generator ),
					Criteria	=> Operation.Criteria,
					Connection	=> Connection,
					Q		=> Q
				);
		APQ.Append( Q, ")" );
	end Append_Operation_Logic_Criteria;

end KOW_Ent.SQL;
