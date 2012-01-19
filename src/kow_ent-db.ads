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
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Finalization;
with Ada.Unchecked_Deallocation;

-------------------
-- KOW Framework --
-------------------
with APQ;
with APQ_Provider;
with KOW_Ent.Data_Storages;

package KOW_Ent.DB_Data_Storages is

	Provider : APQ_Provider.Connection_Provider_Ptr;


	type Root_DB_Storage_Type is abstract new KOW_Ent.Data_Storages.Data_Storage_Type with null record;
	-- this db storage type just implements the data storage



	overriding
	function New_Loader(
				Data_Storage	: in Root_DB_Storage_Type;
				Query		: in Queries.Query_Type
			) return Entity_Loader_Interface'Class;
	-- return an object of the type Root_DB_Entity_Loader_Type'Class





	type Root_DB_Entity_Loader_Type is abstract new KOW_Ent.Data_Storages.Entity_Loader_Interface with null record;
	-- each entity type will have it's on instante of a generic entity loader
	-- @see KOW_Ent.DB.Data_Storages


	overriding
	procedure Execute( Loader : in out Entity_Loader_Interface ) is abstract;
	-- execute the query

	overriding
	procedure Fetch( Loader : in out Entity_Loader_Interface ) is abstract;
	-- fetch the next result


	overriding
	function Has_Next( Loader : in Entity_Loader_Interface ) return Boolean is abstract;
	-- check if there is a next result


	overriding
	procedure Load(
			Loader	: in out Entity_Loader_Interface;
			Entity	: in out Entity_Type'Class
		) is abstract;
	-- load the current query result into the entity
	-- if the entity type is unknown to the loader interface, raises constraint_error



	overriding
	procedure Flush( Loader : in out Entity_Loader_Interface ) is abstract;

end KOW_Ent.DB_Data_Storages;
