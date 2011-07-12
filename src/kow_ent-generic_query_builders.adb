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


--------------
-- Ada 2005 --
--------------
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with Ada.Tags;

-------------------
-- KOW Framework --
-------------------
with KOW_Ent;
with KOW_Ent.Id_Query_Builders;		use KOW_Ent.Id_Query_Builders;
with KOW_Ent.Properties;

---------
-- APQ --
---------
with APQ;
with APQ_Provider;

package body KOW_Ent.Generic_Query_Builders is

	--------------------
	-- Helper Methods --
	--------------------
	procedure Set_Values_From_Query_Helper(
				Entity		: in out Entity_Type'Class;
				Query		: in out APQ.Root_Query_Type'Class;
				Connection	: in out APQ.Root_Connection_Type'Class;
				Info		: in     Entity_Information_Type
			) is
		-- this package hasn't been update to support extended entities...
		-- as a result most of the values aren't set at all.
		--
		-- this procedure a workaround for this ...
		--
		-- it's slow and dumb. I know... but it's easy to implement.
	begin
		KOW_Ent.Set_Values_From_Query(
				Entity		=> Entity,
				Query		=> Query,
				Connection	=> Connection,
				Info		=> Info
			);

		KOW_Ent.Load( Entity, Entity.ID );
		-- the trick is to load the entity :)
	end Set_Values_From_Query_Helper;


	--------------------
	-- Entity Vectors --
	--------------------

	function To_Json_Array( V : in Entity_Vectors.Vector ) return KOW_Lib.Json.Array_Type is
		use Entity_Vectors;
		
		Arr : KOW_Lib.Json.Array_Type;
		procedure Iterator( C : in Cursor ) is
		begin
			KOW_Lib.Json.Append( Arr, To_Json_Object( Element( C ) ) );
		end iterator;
	begin
		Iterate( V, Iterator'Access );

		return Arr;
	end To_Json_Array;
	


	-----------------------------------------------------------------------------------------------------------------

	----------------------
	-- Query Management --
	----------------------
	
	The_Entity_Tag : constant Unbounded_String := To_Unbounded_String( Ada.Tags.Expanded_Name( Entity_Type'Tag ) );

	overriding
	procedure Initialize( Query : in out Entity_Query_Type ) is
	begin
		Prepare( Query, The_Entity_Tag );
	end Initialize;

	-----------------------------------------------------------------------------------------------------------------


	---------------------
	-- Retrieving Data --
	---------------------


	--
	-- All
	--

	function Get_All( Q : in Entity_Query_Type ) return Entity_Vectors.Vector is
		-- get all results from the query
	
		Results : Entity_Vectors.Vector;


		procedure Set_Value(
					Query		: in out APQ.Root_Query_Type'Class;
					Connection	: in out APQ.Root_Connection_Type'Class
				) is
			E : Entity_Type;
		begin
			Set_Values_From_Query_Helper( E, Query, Connection, KOW_Ent.Entity_Registry.Get_Information( Entity_Tag( Q ) ) );
			Entity_Vectors.Append( Results, E );
		end Set_Value;

		procedure Iterate is new Generic_All_Iterator( Set_Value );
	begin
		Iterate( KOW_Ent.Id_Query_Builders.Query_Type( Q ) );
		return Results;
	end Get_All;
	

	--
	-- Some
	--



	function Get_Some(
				Q		: in     Entity_Query_Type;
				From		: in     Positive;
				Limit		: in     Natural
			) return Entity_Vectors.Vector is
		Result		: Entity_Vectors.Vector;
		procedure Set_Value(
					Query		: in out APQ.Root_Query_Type'Class;
					Connection	: in out APQ.Root_Connection_Type'Class
				) is
			Entity	: Entity_Type;
		begin
			Set_Values_From_Query_Helper( Entity, Query, Connection, KOW_Ent.Entity_Registry.Get_Information( Entity_Tag( Q ) ) );
			Entity_Vectors.Append( Result, Entity );
		end Set_Value;

		procedure Iterate is new Generic_Some_Iterator( Set_Value );
	begin
		Iterate( KOW_Ent.Id_Query_Builders.Query_Type( Q ), From, Limit );
		return Result;
	end Get_Some;


	procedure Get_Some(
				Q		: in     Entity_Query_Type;
				From		: in     Positive;
				Limit		: in     Natural;
				Result		:    out Entity_Vectors.Vector;
				Total_Count	:    out Natural
			) is
		-- ger some results from _From_.
		-- if limit = 0 then get all remaining results
		--
		-- for backwards compatibility only! avoid the use
	begin
		Result := Get_Some( Q, From, Limit );
		Total_Count := Count( Q );
	end Get_Some;



	--
	-- First
	--


	function Get_First( Q : in Entity_Query_Type; Unique : in Boolean ) return Entity_Type is
		-- get the first element from the query
		-- if no results, raise NO_ENTITY
		-- if Unique = True and Tuples( Q ) /= 1 then raise DUPLICATED_ENTITY_ELEMENT.
		Result : Entity_Type;

		
		procedure Set_Value(
					Query		: in out APQ.Root_Query_Type'Class;
					Connection	: in out APQ.Root_Connection_Type'Class
				) is
		begin
			Set_Values_From_Query_Helper( Result, Query, Connection, KOW_Ent.Entity_Registry.Get_Information( Entity_Tag( Q ) ) );
		end;

		procedure Iterate is new Generic_First_Iterator( Set_Value );
	begin
		Iterate( KOW_Ent.Id_Query_Builders.Query_Type( Q ), Unique );
		return Result;
	exception
		when APQ.No_Tuple =>
			raise NO_ENTITY with "Tag :: " & To_String( Entity_Tag( Q ) );

	end Get_First;





end KOW_Ent.Generic_Query_Builders;
