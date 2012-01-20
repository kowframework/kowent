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
-- Data Storage definition                                                  --
------------------------------------------------------------------------------



--------------
-- Ada 2005 --
--------------
with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Tags;

-------------------
-- KOW Framework --
-------------------
with APQ;
with KOW_Ent.Queries;

package KOW_Ent.Data_Storages is


	UNICITY_ERROR : Exception;
	-- raised whenever only one result is expected but found more


	NO_RESULT : Exception;
	-- raised whenever there is no result to be loaded



	---------------------------
	-- The Data Storage Type --
	---------------------------

	type Data_Storage_Type is abstract new Data_Storage_Interface with null record;
	-- only extending the API for the Data Storage in an organized way



	--------------
	-- Registry --
	--------------

	function Get_Alias(
				Data_Entity	: in    Data_Storage_Type;
				Entity_Tag	: in    Ada.Tags.Tag
			) return Entity_Alias_Type is abstract;
	-- get the alias for the given entity
	-- for database backend, it's the table name

	---------------------
	-- Store Procedure --
	---------------------

	procedure Store(
				Data_Storage	: in out Data_Storage_Type;
				Entity		: in out Entity_Type'Class
			) is abstract;
	-- store the entity



	--------------------
	-- Load Functions --
	--------------------
	function Load(
				Data_Storage	: in     Data_Storage_Type;
				Filter		: in     Property_Type'Class;
				Unique		: in     Boolean := True
			) return KOW_Ent.Entity_Type'Class;
	-- build the query and then return the first result
	-- if unique=true and there are more results, raise UNICITY_ERROR
	
	function Load(
				Data_Storage	: in     Data_Storage_Type;
				Query		: in     Queries.Query_Type;
				Unique		: in     Boolean := True
			) return KOW_Ent.Entity_Type'Class is abstract;
	-- build the query and then return the first result
	-- if unique=true and there are more results, raise UNICITY_ERROR


	------------
	-- Insert --
	------------
	procedure Insert(
				Data_Storage	: in     Data_Storage_Type;
				Entity		: in out KOW_Ent.Entity_Type'Class
			) is abstract;
	

	------------
	-- Update --
	------------
	procedure Update(
				Data_Storage	: in     Data_Storage_Type;
				Entity		: in out KOW_Ent.Entity_Type'Class;
				Filter		: in     KOW_Ent.Property_Type'Class
			);

	procedure Update(
				Data_Storage	: in     Data_Storage_Type;
				Entity		: in out KOW_Ent.Entity_Type'Class;
				Criteria	: in     KOW_Ent.Queries.Logic_Criteria_Type
			) is abstract;


	-- TODO :: figure out how to load from joined queries
	--function Load(
	--			Data_Storage	: in     Data_Storage_Type;
	--			Query		: in     Queries.Joined_Query_Type;
	--			Unique		: in     Boolean := True
	--		) return KOW_Ent.Entity_Type'Class is abstract;
	-- build the query and then return the first result
	-- if unique=true and there are more results, raise UNICITY_ERROR


	

	------------------------
	-- Central Repository --
	------------------------

	procedure Register_Entity(
				Entity_Tag	: in Ada.Tags.Tag;
				Data_Storage	: in Data_Storage_Ptr
			);
	-- create a relation between an entity and a data storage.

	function Get_Data_Storage( Entity_Tag : in Ada.Tags.Tag ) return Data_Storage_Ptr;



	----------------------------
	-- Entity Query Interface --
	----------------------------


	type Entity_Loader_Interface is interface;
	-- a type that actually deals with query

	
	function New_Loader(
				Data_Storage	: in Data_Storage_Type;
				Query		: in Queries.Query_Type
			) return Entity_Loader_Interface'Class is abstract;
	-- a new query type for the given query representation
	-- the query must be intialized, but not yet executed



	procedure Execute( Loader : in out Entity_Loader_Interface ) is abstract;
	-- execute the query

	procedure Fetch( Loader : in out Entity_Loader_Interface ) is abstract;
	-- fetch the next result

	function Has_Element( Loader : in Entity_Loader_Interface ) return Boolean is abstract;
	-- check if there is fetched element

	procedure Load(
			Loader	: in out Entity_Loader_Interface;
			Entity	: in out Entity_Type'Class
		) is abstract;
	-- load the current query result into the entity
	-- if the entity type is unknown to the loader interface, raises constraint_error


	procedure Flush( Loader : in out Entity_Loader_Interface ) is abstract;
	-- discard all the remaining results
	-- when calling this the query should be ready to executed again.. and again.

	-- typical use of the given interface is:
	--
	-- Loader : Entity_Loader_Interface'Class := New_Loader( Storage, Query );
	-- Entity : Some_Entity_Type;
	-- begin
	--
	-- Execute( Loader );
	--
	-- loop
	-- 	Fetch( Loader );
	-- 	exit when not Has_Element( Loader );
	-- 	Load( Loader, Entity );
	-- 	-- do something with the entity..
	-- end loop;
	--
	--
	-- But this scenario is also valid (even though not recomended):
	--
	-- begin
	--
	-- Execute( Loader );
	-- loop
	-- 	Fetch( Loader );
	-- 	Load( Loader, Entity );
	-- 	...
	-- end loop
	-- exception
	-- 	when No_Result => null;

private


	subtype Entity_Tag_String is Entity_Alias_Type;

	function To_String( Entity_Tag : Ada.Tags.Tag ) return Entity_Tag_String;


	function Hash( Key : in Entity_Tag_String ) return Ada.Containers.Hash_Type;

	package Data_Storage_Maps is new Ada.Containers.Hashed_Maps(
						Key_Type	=> Entity_Tag_String,
						Element_Type	=> Data_Storage_Ptr,
						Equivalent_Keys	=> "=",
						Hash		=> Hash
					);
	
	Storages : Data_Storage_Maps.Map;
end KOW_Ent.Data_Storages;
