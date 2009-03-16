------------------------------------------------------------------------------
--                                                                          --
--                          Ada Works :: Entity                             --
--                                                                          --
--                                Ada Works                                 --
--                                                                          --
--                                 B o d y                                  --
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



-- This is the main package for entity relation
-- Here is where the relation is tracked down into an entity
--
-- The relation can be maped as an entity property and when that's so the user
-- can instanciate a generic package for handling querying related entities.
--
-- @author Marcelo C. de Freitas <marcelo@kow.com.br>

--------------
-- Ada 2005 --
--------------
with Ada.Containers.Vectors;


---------------
-- Ada Works --
---------------
with Aw_Ent;
with Aw_Ent.Properties;



package body Aw_Ent.Relations is


	package body One_To_Many_Relation_Handlers is
		
		-- package Related_Entity_Vectors is new Ada.Containers.Vectors(

		function Get_Query( Entity : in From_Entity_Type ) return Related_Entity_Query_Builders.Query_Type is
			use Related_Entity_Query_Builders;
			Query : Query_Type;
		begin
			Append(
				Q		=> Query,
				Foreign_Key	=> Entity,
				Appender	=> Appender_And,
				Operator	=> Operator_Equals
			);
			return Query;
		end Get_Query;


		function get_All( Entity : in From_Entity_Type ) return Related_Entity_Query_Builders.Entity_Vectors.Vector is
		begin
			return Related_Entity_Query_Builders.Get_All( Q => Get_Query( Entity ) );
		end get_All;
		
		function get_First( Entity : in From_Entity_Type ) return To_Entity_Type is
		begin
			return Related_Entity_Query_Builders.Get_First( Q => Get_Query( Entity ), Unique => False );
		end get_First;

		procedure Store_All(
				Entity			: in out From_Entity_Type;
				Related_Entities	: in out Related_Entity_Query_Builders.Entity_Vectors.Vector
			) is
			
			use Related_Entity_Query_Builders.Entity_Vectors;

			procedure Iterator( C: in Cursor ) is
				E : To_entity_Type := Element( C );
			begin
				Set_Foreign_Key( Entity => E, Key_From => Entity );
				Aw_Ent.Store( E );
				Replace_Element( Related_Entities, C, E );
			end Iterator;
		begin
			Aw_Ent.Store( Entity );
			Iterate( Related_Entities, Iterator'Access );
		end Store_All;

	end One_To_Many_Relation_Handlers;


end Aw_Ent.Relations;
