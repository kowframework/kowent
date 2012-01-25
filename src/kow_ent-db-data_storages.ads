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
-- Main KOW_Ent package.                                                    --
--                                                                          --
-- KOW_Ent is reponsible for handling persistent data in your application   --
-- stored in Database backends using the native DB types.                   --
------------------------------------------------------------------------------



--------------
-- Ada 2005 --
--------------
with Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;
with Ada.Finalization;
with Ada.Tags;
with Ada.Unchecked_Deallocation;

-------------------
-- KOW Framework --
-------------------
with APQ;
with APQ_Provider;
with KOW_Ent.Data_Storages;
with KOW_Ent.Queries;







generic
	type Entity_Type is new KOW_Ent.Entity_Type with private;
	Entity_Alias : String;
package KOW_Ent.DB.Data_Storages is



	type DB_Storage_Type is new KOW_Ent.Data_Storages.Data_Storage_Type with private;



	overriding
	function Get_Alias(
				Data_Entity	: in    DB_Storage_Type;
				Entity_Tag	: in    Ada.Tags.Tag
			) return Entity_Alias_Type;
	-- get the alias for the given entity
	-- for database backend, it's the table name

	overriding
	function Create(
				Data_Storage	: in     DB_Storage_Type;
				Entity_Tag	: in     Ada.Tags.Tag
			) return KOW_Ent.Entity_Ptr;
	-- this factory creates data with no default value changed or whatsoever;
	
	overriding
	procedure Free(
				Data_Storage	: in     DB_Storage_Type;
				Entity		: in out KOW_Ent.Entity_Ptr
			);

	--------------------
	-- Load Functions --
	--------------------
	

	overriding
	procedure Load(
				Data_Storage	: in     DB_Storage_Type;
				Query		: in     Queries.Query_Type;
				Entity		: in out KOW_Ent.Entity_Type'Class;
				Unique		: in     Boolean := True
			);
	-- build the query and then return the first result
	-- if unique=true and there are more results, raise UNICITY_ERROR


	------------
	-- Insert --
	------------

	overriding
	procedure Insert(
				Data_Storage	: in     DB_Storage_Type;
				Entity		: in out KOW_Ent.Entity_Type'Class
			);
	

	------------
	-- Update --
	------------

	overriding
	procedure Update(
				Data_Storage	: in     DB_Storage_Type;
				Entity		: in out KOW_Ent.Entity_Type'Class;
				Criteria	: in     KOW_Ent.Queries.Logic_Criteria_Type
			);



	overriding
	function New_Loader(
				Data_Storage	: in     DB_Storage_Type;
				Query		: in     KOW_Ent.Queries.Query_Type'class
			) return KOW_Ent.Data_Storages.Entity_Loader_Interface'Class;

	

	-------------------
	-- Entity Loader --
	-------------------

	type DB_Loader_Type is new Ada.Finalization.Controlled and KOW_Ent.Data_Storages.Entity_Loader_Interface with private;


	overriding
	procedure Adjust( Loader : in out DB_Loader_Type );
	-- reallocate the pointers :)

	overriding
	procedure Finalize( Loader : in out DB_Loader_Type );
	-- make sure we don't leave garbage in the memory


	overriding
	procedure Execute( Loader : in out DB_Loader_Type );
	-- execute the query

	overriding
	procedure Fetch( Loader : in out DB_Loader_Type );
	-- fetch the next result


	overriding
	function Has_Element( Loader : in DB_Loader_Type ) return Boolean;
	-- check if there is a next result


	overriding
	procedure Load(
			Loader	: in out DB_Loader_Type;
			Entity	: in out KOW_Ent.Entity_Type'Class
		);
	-- load the current query result into the entity
	-- if the entity type is unknown to the loader interface, raises constraint_error



	overriding
	procedure Flush( Loader : in out DB_Loader_Type );


private


	type DB_Storage_Type is new KOW_Ent.Data_Storages.Data_Storage_Type with null record;

	type Value_Container_Type is new Ada.Finalization.Controlled with record
		Value : Value_Ptr;
	end record;


	overriding
	procedure Adjust( V : in out Value_Container_Type );

	overriding
	procedure Finalize( V: in out Value_Container_Type );
	

	package Value_Lists is new Ada.Containers.Doubly_Linked_Lists( Value_Container_TYpe );
	-- I've tested with array of fixed length as well
	-- and the curious thing is that... well, the fixed-lenthg array wasn't significantly faster
	-- so, the doubly linked list a somewhat whise choice then :)
	--
	-- in some cases it's probably - a lot - faster to use cursors; the cursor would have a randomized name.
	-- for that we would have to:
	-- 	1. find out if a cursor can be reused by another connection
	-- 	2. implement efficient cursor in every APQ supported driver
	--
	-- but thanks to how APQ_Provider works, it's probably a bad idea

	package Entity_Values_Lists is new Ada.Containers.Doubly_Linked_Lists(
									Element_Type	=> Value_Lists.List,
								       	"="		=> Value_lists."="
							      	);
	
	function Hash( Tag : in Ada.Tags.Tag ) return Ada.Containers.Hash_Type;
	package Join_Entity_Values_Maps is new Ada.Containers.Hashed_Maps(
									Key_Type	=> Ada.Tags.Tag,
									Element_Type	=> Entity_Values_Lists.List,
									Hash		=> Hash,
									Equivalent_Keys	=> Ada.Tags."=",
									"="		=> Entity_Values_Lists."="
								);
	package Join_Entity_Cursor_Maps is new Ada.Containers.Hashed_Maps(
									KEy_Type	=> Ada.Tags.Tag,
									Element_Type	=> Entity_Values_Lists.Cursor,
									Hash		=> Hash,
									Equivalent_Keys	=> Ada.Tags."=",
									"="		=> Entity_Values_Lists."="
								);

	type DB_Loader_Type is new Ada.Finalization.Controlled and KOW_Ent.Data_Storages.Entity_Loader_Interface with record
		Cache		: Entity_Values_Lists.List;
		-- where the results are cached when execute is called as APQ_Provide require
		-- us to fetch all the results at once and then release the connection for good

		Join_Cache	: Join_Entity_Values_Maps.Map;
		-- in case it's a join query... that's where the joined values are stored
		Join_Cursors	: Join_Entity_Cursor_Maps.Map;

		Current		: Entity_Values_Lists.Cursor := Entity_Values_Lists.No_Element;

		Query		: KOW_Ent.Queries.Query_Ptr;
	end record;


	THE_Entity_Alias : Entity_Alias_Type;
	-- where the entity alias is actually stored (it's copied from the Entity_Alias string on elaboration time)


	Storage : aliased DB_Storage_Type;
	-- the actual data storage instance is a singleton object for each entity type (at least for entities in the database backend)
end KOW_Ent.DB.Data_Storages;
