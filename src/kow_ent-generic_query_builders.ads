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
with KOW_Ent.Id_Query_Builders;
with KOW_Lib.Json;
with APQ;

generic
	type Entity_Type is new KOW_Ent.Entity_Type with private;
package KOW_Ent.Generic_Query_Builders is

	
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

	-----------------------------------------------------------------------------------------------------------------

	----------------------
	-- Query Management --
	----------------------
	
	type Entity_Query_Type is new KOW_Ent.Id_Query_Builders.Query_Type with null record;


	overriding
	procedure Prepare( Q : in out Entity_Query_Type; Entity_Tag : in Ada.Tags.Tag );
	-- clear the query 
	
	overriding
	procedure Prepare( Q : in out Entity_Query_Type; Entity_Tag : in Unbounded_String );
	-- clear the query


	overriding
	function Entity_Tag( Q : in Entity_Query_Type ) return Unbounded_String;
	-- return always the package entity tag




	-----------------------------------------------------------------------------------------------------------------



	---------------------
	-- Retrieving Data --
	---------------------

	--
	-- All
	--


	function Get_All( Q : in Entity_Query_Type ) return Entity_Vectors.Vector;
	-- get all results from the query
	-- if no results, raise NO_ENTITY

	--
	-- Some
	--

	function Get_Some(
				Q		: in     Entity_Query_Type;
				From		: in     Positive;
				Limit		: in     Natural
			) return Entity_Vectors.Vector;
	-- ger some results from _From_.
	-- if limit = 0 then get all remaining results



	procedure Get_Some(
				Q		: in     Entity_Query_Type;
				From		: in     Positive;
				Limit		: in     Natural;
				Result		:    out Entity_Vectors.Vector;
				Total_Count	:    out Natural
			);
	-- ger some results from _From_.
	-- if limit = 0 then get all remaining results
	--
	-- for backwards compatibility only! avoid the use


	--
	-- First
	--

	function Get_First( Q : in Entity_Query_Type; Unique : in Boolean ) return Entity_Type;
	-- get the first element from the query
	-- if Unique = True and Tuples( Q ) /= 1 then raise DUPLICATED_ENTITY_ELEMENT.
	-- if no results, raise NO_ENTITY




end KOW_Ent.Generic_Query_Builders;
