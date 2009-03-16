------------------------------------------------------------------------------
--                                                                          --
--                          Ada Works :: Entity                             --
--                                                                          --
--                                Ada Works                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--               Copyright (C) 2007-2009, Ada Works Project                 --
--                                                                          --
--                                                                          --
-- Aw_Lib is free library;  you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. Aw_Lib is distributed in the hope that it will be useful, but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with Aw_Lib; see file COPYING. If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
------------------------------------------------------------------------------


-- This is a generic package to assist building queries for retrieving your
-- data from the Database
--
-- @author Marcelo C. de Freitas <mfreitas@ydeasolutions.com.br>


--------------
-- Ada 2005 --
--------------
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;

---------------
-- Ada Works --
---------------
with Aw_Ent;
with APQ;

generic
	type Entity_Type is new Aw_Ent.Entity_Type with private;
package Aw_Ent.Query_Builders is
	
	-----------------------------------
	-- Generic Package Instantiation --
	-----------------------------------

	package Entity_Vectors is new Ada.Containers.Vectors(
				Element_Type	=> Entity_Type,
				Index_Type	=> Positive
			);
	

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
		Operator_Equals,
		Operator_Not_Equals,
		Operator_Like
	);

	----------------------
	-- Query Management --
	----------------------
	
	type Query_Type is private;
	type Query_Ptr is access Query_Type;


	procedure Append(
				Q		: in out Query_Type;
				Foreign_Key	: in     Aw_Ent.Entity_Type'Class;
				Appender	: in     Logic_Appender := Appender_AND;
				Operator	: in     Logic_Operator := Operator_Equals
			);
	-- append a query element based on foreign key
	

	procedure Append(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     Aw_Ent.Id_Type;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equals
			);
	-- append a query element to this query

	procedure Append(
				Q	: in out Query_Type;
				Column	: in     Unbounded_String;
				Value	: in     Aw_Ent.Id_Type;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equals
			);


	procedure Append(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     String;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equals
			);
	-- append a query element to this query


	procedure Append(
				Q	: in out Query_Type;
				Column	: in     String;
				Value	: in     Unbounded_String;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equals
			);
	-- append a query element to this query

	
	procedure Append(
				Q	: in out Query_Type;
				Column	: in     Unbounded_String;
				Value	: in     Unbounded_String;
				Appender: in     Logic_Appender := Appender_AND;
				Operator: in     Logic_Operator := Operator_Equals
			);
	-- append a query element to this query
	

	procedure Append(
				Q	: in out Query_Type;
				Child_Q	: in     Query_Type;
				Appender: in     Logic_Appender := Appender_AND
			);
	-- append another query as a child query :: () stuff
	


	procedure Append_to_APQ_Query(
				Q	: in     Query_Type;
				APQ_Q	: in out APQ.Root_Query_Type'Class
			);


	function Get_All( Q : in Query_Type ) return Entity_Vectors.Vector;
	-- get all results from the query
	-- if no results, raise NO_ENTITY
	

	function Get_First( Q : in Query_Type; Unique : in Boolean ) return Entity_Type;
	-- get the first element from the query
	-- if Unique = True and Tuples( Q ) /= 1 then raise DUPLICATED_ENTITY_ELEMENT.
	-- if no results, raise NO_ENTITY

private

	type Control_Operation_Type is (
			L_Operator,	-- Logical Operator
			Q_Operator	-- Child Query;
		);
	type Operator_Handler_Type is record
		-- global :
		Appender	: Logic_Appender;
		Operation_Type	: Control_Operation_Type;

		-- logical :
		Column		: Unbounded_String;
		Value		: Unbounded_String;
		Operator	: Logic_Operator;

		-- child query :
		Child_Query	: Query_Ptr;
	end record;

	package Operator_Vectors is new Ada.Containers.Vectors(
			Element_Type	=> Operator_Handler_Type,
			Index_Type	=> Positive
		);
	-- used to store operators in the query


	type Query_Type is record
		Operators	: Operator_Vectors.Vector;
	end record;

end Aw_Ent.Query_Builders;
