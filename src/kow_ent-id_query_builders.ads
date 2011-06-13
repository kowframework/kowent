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
-- This is a generic package to assist building queries for retrieving your --
-- data from the Database                                                   --
------------------------------------------------------------------------------


-- TODO :: make it support extended entity types.. so far it works fine for
-- types derived from the KOW_Ent.Entity_Type directly!


--------------
-- Ada 2005 --
--------------
with Ada.Containers.Vectors;
with Ada.Finalization;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with Ada.Tags;

-------------------
-- KOW Framework --
-------------------
with KOW_Ent;
with KOW_Ent.Extra_Properties;		use KOW_Ent.Extra_Properties;
with KOW_Lib.Json;
with APQ;

package KOW_Ent.ID_Query_Builders is


	----------------
	-- Exceptions --
	----------------
	NO_ENTITY			: Exception;
	DUPLICATED_ENTITY_ELEMENT	: Exception;

	-------------------
	-- Control Types --
	-------------------

	type Logic_Appender is (
		Appender_AND,
		Appender_OR
	);

	-- all operators take into account:
	-- 	Left-hand side of operation	=> Value inside the database
	-- 	Right-hand side of operation	=> Parameter passed into the append procedure
	type Logic_Operator is (
		Operator_Equal_To,
		Operator_Not_Equal_To,
		Operator_Like,
		Operator_Less_Than,
		Operator_Less_Than_Or_Equal_To,
		Operator_Greater_Than,
		Operator_Greater_Than_Or_Equal_To
	);


	-----------------------------------------------------------------------------------------------------------------

	----------------------
	-- Query Management --
	----------------------
	
	type Query_Type is tagged private;
	type Query_Ptr is access Query_Type;


	function To_String( Q : in Query_Type ) return String;
	-- return the query as string


	procedure Clear( Q : in out Query_Type );
	-- clear the query for reuse


	procedure Prepare( Q : in out Query_Type; Entity_Tag : in Ada.Tags.Tag );
	-- clear the query and set it up for the given entity tag
	
	procedure Prepare( Q : in out Query_Type; Entity_Tag : in Unbounded_String );
	-- clear the query and set it up for the given entity tag

	function Entity_Tag( Q : in Query_Type ) return Unbounded_String;
	-- return the entity type for the given query type


	--
	-- Foreign Key
	--
	procedure Append(
				Q		: in out Query_Type;
				Foreign_Key	: in     KOW_Ent.Entity_Type'Class;
				Appender	: in     Logic_Appender := Appender_AND;
				Operator	: in     Logic_Operator := Operator_Equal_To
			);
	-- append a query element based on foreign key
	
	--
	-- KOW_Ent.ID_Type
	--

	procedure Append(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     KOW_Ent.Id_Type;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			);
	-- append a query element to this query

	procedure Append(
				Q	: in out Query_Type;
				Column	: in     Unbounded_String;
				Value	: in     KOW_Ent.Id_Type;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			);

	--
	-- String
	--

	procedure Append(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     String;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			);
	-- append a query element to this query


	procedure Append(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     Unbounded_String;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			);
	-- append a query element to this query

	
	procedure Append(
				Q	: in out Query_Type;
				Column	: in     Unbounded_String;
				Value	: in     Unbounded_String;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			);
	-- append a query element to this query
	

	-- 
	-- Password
	--
	
	procedure Append_Password(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     String;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			);
	-- append a query element to this query


	procedure Append_Password(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     Unbounded_String;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			);
	-- append a query element to this query

	
	procedure Append_Password(
				Q	: in out Query_Type;
				Column	: in     Unbounded_String;
				Value	: in     Unbounded_String;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			);
	-- append a query element to this query


	--
	-- Percent 
	-- 

	procedure Append(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     Percent;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			);


	--
	-- Money 
	--

	procedure Append(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     Money;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			);


	--
	-- Date 
	--

	procedure Append_Date(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     Date;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			);


	--
	-- Timestamp 
	--

	procedure Append_Timestamp(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     Timestamp;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			);


	--
	-- Dimension 
	--


	procedure Append(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     Dimension;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			);



	--
	-- Weight 
	--


	procedure Append(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     Weight;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			);



	--
	-- Count 
	--


	procedure Append(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     KOW_Ent.Extra_Properties.Count;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equal_To
			);







	--
	-- Sub Queries
	--


	procedure Append(
				Q	: in out Query_Type;
				Child_Q	: in     Query_Type;
				Appender: in     Logic_Appender := Appender_AND
			);
	-- append another query as a child query :: () stuff
	

	--------------
	-- Order By --
	--------------

	type Ordenation_Type is ( ASCENDING, DESCENDING );
	-- order ascending or descending?

	
	procedure Append_Order(
				Q		: in out Query_Type;
				Column		: in     Unbounded_String;
				Ordenation	: in     Ordenation_Type := ASCENDING
		 	);
	procedure Append_Order(
				Q		: in out Query_Type;
				Column		: in     String;
				Ordenation	: in     Ordenation_Type := ASCENDING
		 	);


	-----------------------------------------------------------------------------------------------------------------



	-----------------------------
	-- Generic Implementations --
	-----------------------------


	-- 
	-- All
	--
	generic
		with procedure Iterator(
					Query		: in out APQ.Root_Query_Type'Class;
					Connection	: in out APQ.Root_Connection_Type'Class
				);
	procedure Generic_All_Iterator( Q : in Query_Type );

	-- 
	-- Some
	--

	generic
		with procedure Iterator(
					Query		: in out APQ.Root_Query_Type'Class;
					Connection	: in out APQ.Root_Connection_Type'Class
				);
	procedure Generic_Some_Iterator(
				Q	: in Query_Type;
				From	: in Positive;
				Limit	: in Natural
			);

	--
	-- First 
	--
	generic
		with procedure Iterator(
				Query		: in out APQ.Root_Query_Type'Class;
				Connection	: in out APQ.Root_Connection_Type'Class
			);
	procedure Generic_First_Iterator(
				Q	: in Query_Type;
				Unique	: in Boolean
			);



	---------------------
	-- Retrieving Data --
	---------------------

	--
	-- All
	--

	function Get_All( Q : in Query_Type ) return KOW_Ent.ID_Array_Type;
	-- get all results from the query
	-- if no results, raise NO_ENTITY


	--
	-- Some
	--

	function Get_Some(
				Q		: in     Query_Type;
				From		: in     Positive;
				Limit		: in     Natural
			) return KOW_Ent.Id_Array_Type;
	-- ger some results from _From_.
	-- if limit = 0 then get all remaining results


	--
	-- First
	--

	function Get_First( Q : in Query_Type; Unique : in Boolean ) return KOW_Ent.ID_Type;
	-- get the first element from the query
	-- if Unique = True and Tuples( Q ) /= 1 then raise DUPLICATED_ENTITY_ELEMENT.
	-- if no results, raise NO_ENTITY


	--
	-- Count
	--

	function Count( Q : in Query_Type ) return Natural;
	-- count how many results this query would return

	-----------------------------------------------------------------------------------------------------------------


private
	------------------------
	-- SQL Query Building --
	------------------------


	procedure Build_Query(
				Q		: in      Query_Type;
				Query		: in out APQ.Root_Query_Type'Class;
				Connection	: in out APQ.Root_Connection_Type'Class;
				Count_Query	: in     Boolean := False
			);

	procedure Build_Query(
				Q		: in     Query_Type;
				Query		: in out APQ.Root_Query_Type'Class;
				Connection	: in out APQ.Root_Connection_Type'Class;
				From		: in     Positive;
				Limit		: in     Natural
			);


	procedure Append_to_APQ_Query(
				Q		: in     Query_Type;
				APQ_Q		: in out APQ.Root_Query_Type'Class;
				Connection	: in out APQ.Root_Connection_Type'Class
			);

	procedure Append_Order_by_to_APQ_Query(
				Q		: in     Query_Type;
				APQ_Q		: in out APQ.Root_Query_Type'Class;
				Connection	: in out APQ.Root_Connection_Type'Class
			);


	---------------
	-- Operators --
	---------------

	type Control_Operation_Type is (
			L_Operator,	-- Logical Operator
			Q_Operator	-- Child Query;
		);

	type Operator_Data_Type is (
			Is_String,

			Is_Percent,
			Is_Money,
			Is_Date,
			Is_Timestamp,
			Is_Dimension,
			Is_Weight,
			Is_Count,

			Is_None		-- used by the Q_Operator
		);

	
	type Query_Container_Type is new Ada.Finalization.Controlled with record
		Q	: Query_Ptr := null;
	end record;

	overriding
	procedure Finalize( Container : in out Query_Container_Type );

	overriding
	procedure Adjust( Container : in out Query_Container_Type );



	type Operator_Handler_Type( Data_Type : Operator_Data_Type := Is_None ) is record
		-- global :
		Appender	: Logic_Appender;
		Operation_Type	: Control_Operation_Type;

		-- logical :
		Column		: Unbounded_String;
		Operator	: Logic_Operator;

		-- child query :
		Child_Query	: Query_Container_Type;


		case Data_Type is
			when Is_String =>
				String_Value : Unbounded_String;
			when Is_Percent =>
				Percent_Value : Percent;
			when Is_Money =>
				Money_Value : Money;
			when Is_Date =>
				Date_Value : Date;
			when Is_Timestamp =>
				Timestamp_Value : Timestamp;
			when Is_Dimension =>
				Dimension_Value : Dimension;
			when Is_Weight =>
				Weight_Value : Weight;
			when Is_Count =>
				Count_Value : KOW_Ent.Extra_Properties.Count;

			when Is_None =>
				null;
		end case;
	end record;



	package Operator_Vectors is new Ada.Containers.Vectors(
			Element_Type	=> Operator_Handler_Type,
			Index_Type	=> Positive
		);
	-- used to store operators in the query




	--------------
	-- Order By --
	--------------
	

	type Order_By_Type is record
		Column		: Unbounded_String;
		Ordenation	: Ordenation_Type;
	end record;

	package Order_by_Vectors is new Ada.Containers.Vectors(
			Element_Type	=> Order_By_Type,
			Index_Type	=> Positive
		);

	type Query_Type is tagged record
		Operators	: Operator_Vectors.Vector;
		Order_By	: Order_By_Vectors.Vector;
		The_Entity_Tag	: Unbounded_String;
	end record;


end KOW_Ent.ID_Query_Builders;
