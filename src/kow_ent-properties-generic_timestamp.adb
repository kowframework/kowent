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

with APQ;


package body KOW_ENT.Properties.Generic_Timestamp is



	
	------------------
	-- The Property --
	------------------


	overriding
	procedure Set_Property(	
				Property	: in     Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Q		: in out APQ.Root_Query_Type'Class;	-- the query from witch to fetch the result
				Connection	: in out APQ.Root_Connection_Type'Class -- the connection that belongs the query
			) is
		-- Set the property from the query into the Entity.
		Column : String := To_String( Property.Column_Name );
		Index  : APQ.Column_Index_Type := APQ.Column_Index( Q, Column );
	begin
		Property.Setter.all(
				Entity,
				Value( Q, Index )
			);
		exception
			when APQ.Null_Value =>
				Property.Setter.all(
						Entity,
						Null_Value
					);
	end Set_Property;



	overriding
	procedure Get_Property(
				Property	: in     Property_Type;		-- the property worker
				Entity		: in     Entity_Type'Class;		-- the entity
				Query		: in out APQ.Root_Query_Type'Class;	-- the query to witch append the value to insert
				Connection	: in out APQ.Root_Connection_Type'Class	-- the connection that belongs the query
			) is
		-- Append into a query being created by the main KOW_ent engine.
	begin
		Append( Query, Property.Getter.all( Entity ) );
	end Get_Property;



	overriding
	procedure Set_Property(
				Property	: in     Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Value		: in     String				-- the String representation of this value
			) is
		-- Set the property from a String representation of the value
	begin
		Property.Setter.all( Entity, From_String( Value ) );
	end Set_Property;



	overriding
	function Get_Property(
				Property	: in     Property_Type;		-- the property worker
				Entity		: in     Entity_Type'Class		-- the entity
			) return String is
	begin
		return To_String( Property.Getter.all( Entity ) );
	end Get_property;




	overriding
	procedure Append_Create_Table(
				Property	: in     Property_Type;
				Query		: in out APQ.Root_Query_Type'Class
			) is
	begin
		APQ.Append(
				Query,
				To_String( Property.Column_Name ) & " DATETIME NOT NULL"
			);

		-- DATETIME has a larger range and is not stored in UTC in the database backend
		-- This will easy the data processing and also the entire Ada range for Ada.Calendar.Time fits into
		-- the database
	end Append_Create_Table;


	function New_Property(
					Column_Name	: in String;
					Getter		: Getter_Callback;
					Setter		: Setter_Callback;
					Immutable	: in Boolean := False
			) return KOW_Ent.Entity_Property_Ptr is
		PT : Property_Type;
	begin
		PT.Column_Name	:= To_Unbounded_String( Column_Name );
		PT.Getter	:= Getter;
		PT.Setter	:= Setter;
		PT.Immutable	:= Immutable;
		return new Property_Type'( PT );
	end New_Property;

end KOW_Ent.Properties.Generic_Timestamp;
