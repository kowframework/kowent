-- This package contains all from factory supported property type handlers.
-- 
--
-- NOTE :: for now the only type implemented is Unbounded_String
-- TODO :: implement support for more types.




with APQ;

with Aw_Ent;


package Aw_Ent.Properties is

	-------------------------------
	-- Unbounded String property --
	-------------------------------

	type UString_Getter_Type is not null access function(
				Entity	: in Aw_Ent.Entity_Type'Class
			) return Unbounded_String;
	type UString_Setter_Type is not null access procedure(
				Entity	: in Aw_Ent.Entity_Type'Class;
				Value	: in Unbounded_String
			);

	type UString_Property_Type is new Entity_Property_Type with private;

	overriding
	procedure Set_Property(	
				Property	: in     UString_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Q		: in out APQ.Root_Query_Type'Class;	-- the query from witch to fetch the result
				Connection	: in     Aw_Ent.Connection_Ptr		-- the connection that belongs the query
			);
	-- Set the property into the Entity.

	overriding
	procedure Get_Property(
				Property	: in     UString_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Query		: in out APQ.Root_Query_Type'Class;	-- the query to witch append the value to insert
				Connection	: in     Aw_Ent.Connection_Ptr		-- the connection that belongs the query
			);


	function New_UString_Property(
				Column_Name	: in     String;
				Getter		: in     UString_Getter_Type;
				Setter		: in     UString_Setter_Type
			) return Entity_Property_Ptr;
	-- used to assist the creation of UString properties.
private

	type UString_Property_Type is new Entity_Property_Type with record
		Getter : UString_Getter_Type;
		Setter : UString_Setter_Type;
	end record;
end Aw_Ent.Properties;
