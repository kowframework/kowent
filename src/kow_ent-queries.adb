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
-- Contains methods for representing a query into the data storage          --
-- The query should support:                                                --
--      1. multiple logical operations with multiple levels:                --
--             a < b && ( a = b || c <= d)                                  --
--      2. Joins (left/right/inner)                                         --
------------------------------------------------------------------------------




--------------
-- Ada 2005 --
--------------
with Ada.Finalization;
with Ada.Tags;

package body KOW_Ent.Queries is

	-------------------------
	-- Logic Criteria Type --
	-------------------------

	overriding
	procedure Adjust( Logic_Criteria : in out Logic_Criteria_Type ) is
		use Logic_Relation_Lists;
		-- reallocate all the operations
		New_Operations : List;

		procedure Iterator( C : in Cursor ) is
		begin
			Append( New_Operations, Clone( Element( C ).all ) );
		end Iterator;
	begin
		Iterate( Logic_Criteria.Operations, Iterator'Access );
		Logic_Criteria.Operations := New_Operations;
	end Adjust;

	overriding
	procedure Finalize( Logic_Criteria : in out Logic_Criteria_Type ) is
		-- free the operations
		use Logic_Relation_Lists;

		procedure Iterator( C : in Cursor ) is
			Ptr : Logic_Relation_Ptr := Element( C );
		begin
			Free( Ptr.all, Ptr );
		end Iterator;
	begin
		Iterate( Logic_Criteria.Operations, Iterator'Access );
		Clear( Logic_Criteria.Operations );
	end Finalize;


	procedure Append(
				Criteria	: in out Logic_Criteria_Type;
				Operation	: in     Logic_Relation_Type'Class
			) is
	begin
		Logic_Relation_Lists.Append(
						Criteria.Operations,
						Clone( Operation )
					);
	end Append;

	
	procedure Iterate(
				Criteria	: in     Logic_Criteria_Type;
				Iterator	: access procedure( Operation : in Logic_Relation_Type'Class )
			) is
		procedure Inner_Iterator( C : Logic_Relation_Lists.Cursor ) is
		begin
			Iterator.all( Logic_Relation_Lists.Element( C ).all );
		end Inner_Iterator;
	begin
		Logic_Relation_Lists.Iterate( Criteria.Operations, Inner_Iterator'Access );
	end Iterate;


	function Is_Empty( Criteria : in Logic_Criteria_Type ) return Boolean is
	begin
		return Logic_Relation_Lists.Is_Empty( Criteria.Operations );
	end Is_Empty;
	-- check if there is any operation appended into the criteria


	----------------------------
	-- Finally The Query Type --
	----------------------------


	type Query_Access is access all Query_Type;
	procedure Free is new Ada.Unchecked_Deallocation( Name => Query_Access, Object => Query_Type );


	function Clone( Query : in Query_Type ) return Query_Ptr is
		Q : Query_Access := new Query_Type'( Query );
	begin
		return Query_Ptr( Q );
	end Clone;

	procedure Free( Query : in Query_Type; Name : in out Query_Ptr ) is
	begin
		if Name /= null then
			Free( Query_Access( Name ) );
		end if;
	end Free;

	---------------------------------
	-- And now the Join Query Type --
	---------------------------------


	type Join_Query_Access is access all Join_Query_Type;
	procedure Free is new Ada.Unchecked_Deallocation( Name => Join_Query_Access, Object => Join_Query_Type );

	overriding
	function Clone( Query : in Join_Query_Type ) return Query_Ptr is
		Q : Join_Query_Access := new Join_Query_Type'( Query );
	begin
		return Query_Ptr( Q );
	end Clone;

	overriding
	procedure Free( Query : in Join_Query_Type; Name : in out Query_Ptr ) is
	begin
		if Name /= null then
			Free( Join_Query_Access( Name ) );
		end if;
	end Free;

	procedure Append( 
				Join_Query	: in out Join_Query_Type;
				Join_Description: in     Join_Description_Type
			) is
		-- append a join description type
	begin
		Join_Description_Lists.Append( Join_Query.Joins, Join_Description );
	end Append;

	procedure Iterate(
				Join_Query	: in Join_Query_Type;
				Iterator	: access procedure( Description : in Join_Description_Type )
			) is
		-- iterate over all the appended descriptions
		use Join_Description_Lists;
		procedure Inner_Iterator( C : Cursor ) is
		begin
			Iterator.all( Element( C ) );
		end Inner_iterator;
	begin
		Iterate( Join_Query.Joins, Inner_Iterator'Access );
	end Iterate;
end KOW_Ent.Queries;
