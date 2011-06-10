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

-------------------
-- KOW Framework --
-------------------
with KOW_Ent;
with KOW_Ent.Extra_Properties;		use KOW_Ent.Extra_Properties;
with KOW_Lib.Json;
with APQ;

generic
	type Entity_Type is new KOW_Ent.Entity_Type with private;
package KOW_Ent.Query_Builders is
	
	--------------------
	-- Entity Vectors --
	--------------------

	package Entity_Vectors is new Ada.Containers.Vectors(
				Element_Type	=> Entity_Type,
				Index_Type	=> Positive
			);
	

	function To_Json_Array( V : in Entity_Vectors.Vector ) return KOW_Lib.Json.Array_Type;
	
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

	type Logic_Operator is (
		Operator_Equal_To,
		Operator_Not_Equal_To,
		Operator_Like,
		Operator_Less_Than,
		Operator_Less_Than_Or_Equal_To,
		Operator_Greater_Than,
		Operator_Greater_Than_Or_Equal_To
	);

	----------------------
	-- Query Management --
	----------------------
	
	type Query_Type is private;
	type Query_Ptr is access Query_Type;


	procedure Clear( Q : in out Query_Type );
	-- clear the query for reuse

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

	--
	-- Date 
	--

	--
	-- Timestamp 
	--

	--
	-- Dimension 
	--

	--
	-- Weight 
	--

	--
	-- Count 
	--








	--
	-- Sub Queries
	--


	procedure Append(
				Q	: in out Query_Type;
				Child_Q	: in     Query_Type;
				Appender: in     Logic_Appender := Appender_AND
			);
	-- append another query as a child query :: () stuff
	

	-- -------- --
	-- Order By --
	-- -------- --

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


	--------------------
	-- IMPLEMENTATION --
	--------------------

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

	function Get_All( Q : in Query_Type ) return Entity_Vectors.Vector;
	-- get all results from the query
	-- if no results, raise NO_ENTITY
	
	procedure Get_Some(
				Q		: in     Query_Type;
				From		: in     Positive;
				Limit		: in     Natural;
				Result		:    out Entity_Vectors.Vector;
				Total_Count	:    out Natural
			);
	-- get a page of results from From (inclusive), presenting at max Ammount results.
	-- also, count the total results for the user...
	-- if limit = 0, get all results


	function Get_Some(
				Q		: in     Query_Type;
				From		: in     Positive;
				Limit		: in     Natural
			) return Entity_Vectors.Vector;


	function Get_First( Q : in Query_Type; Unique : in Boolean ) return Entity_Type;
	-- get the first element from the query
	-- if Unique = True and Tuples( Q ) /= 1 then raise DUPLICATED_ENTITY_ELEMENT.
	-- if no results, raise NO_ENTITY


	function To_String( Q : in Query_Type ) return String;
	-- return the query as string

private

	-- --------- --
	-- Operators --
	-- --------- --

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




	-- -------- --
	-- Order By --
	-- -------- --
	

	type Order_By_Type is record
		Column		: Unbounded_String;
		Ordenation	: Ordenation_Type;
	end record;

	package Order_by_Vectors is new Ada.Containers.Vectors(
			Element_Type	=> Order_By_Type,
			Index_Type	=> Positive
		);

	type Query_Type is record
		Operators	: Operator_Vectors.Vector;
		Order_By	: Order_By_Vectors.Vector;
	end record;


end KOW_Ent.Query_Builders;
