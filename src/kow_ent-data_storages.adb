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
-- Data Storage definition                                                  --
------------------------------------------------------------------------------



--------------
-- Ada 2005 --
--------------
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Finalization;
with Ada.Strings.Hash;
with Ada.Unchecked_Deallocation;

-------------------
-- KOW Framework --
-------------------
with APQ;
with KOW_Ent.Queries;
with KOW_Ent.Queries.Logic_Relations;

package body KOW_Ent.Data_Storages is


	--------------------
	-- Load Functions --
	--------------------
	procedure Load(
				Data_Storage	: in     Data_Storage_Type;
				Filter		: in     Property_Type'Class;
				Entity		: in out Entity_Type'Class;
				Unique		: in     Boolean := True
			) is
		use Queries;

		Q	: Query_Type;
		Op	: Logic_Relations.Stored_Vs_Value_Operation;
	begin
		Op.Property_Name := Filter.Name;
		Op.Value := new Value_Type'(Get_Value( Filter ));
		-- build the query and then return the type
	
		Append( Q.Logic_Criteria, Op );

		Load( Data_Storage_Type'Class( Data_Storage ), Q, Entity, Unique );
	end Load;

	------------
	-- Update --
	------------
	procedure Update(
				Data_Storage	: in     Data_Storage_Type;
				Entity		: in out KOW_Ent.Entity_Type'Class;
				Filter		: in     KOW_Ent.Property_Type'Class
			) is
		use KOW_Ent.Queries;
		use KOW_Ent.Queries.Logic_Relations;


		Operation : Logic_Relations.Stored_Vs_Value_Operation;
		Criteria  : Logic_Criteria_Type;

	begin
		Operation.Entity_Tag	:= Entity'Tag;
		Operation.Property_Name	:= Filter.Name;
		Operation.Value		:= new Value_Type'( Get_Value( Filter ) );
		Operation.Relation	:= Relation_Equal_To;
		Operation.Operator	:= Operator_AND;

		Append( Criteria, Operation );


		Update(
				Data_Storage	=> Data_Storage_Type'Class( Data_Storage ),
				Entity		=> Entity,
				Criteria	=> Criteria
			);
	end Update;

	
	------------------------
	-- Central Repository --
	------------------------

	procedure Register_Entity(
				Entity_Tag	: in Ada.Tags.Tag;
				Data_Storage	: in Data_Storage_Ptr
			) is
		-- create a relation between an entity and a data storage.
		use Data_Storage_Maps;
	begin
		Insert( Storages, To_String( Entity_Tag ), Data_Storage );
	end Register_Entity;

	function Get_Data_Storage( Entity_Tag : in Ada.Tags.Tag ) return Data_Storage_Ptr is
		use Data_Storage_Maps;
	begin
		return Element( Storages, To_String( Entity_Tag ) );
	end Get_Data_Storage;


-- private

	function To_String( Entity_Tag : Ada.Tags.Tag ) return Entity_Tag_String is
	begin
		return To_Alias( Ada.Tags.Expanded_Name( Entity_Tag ) );
	end To_String;



	function Hash( Key : in Entity_Tag_String ) return Ada.Containers.Hash_Type is
	begin
		return Ada.Strings.Hash( String( Key ) );
	end Hash;


end KOW_Ent.Data_Storages;
