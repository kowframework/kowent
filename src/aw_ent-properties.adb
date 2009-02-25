-- This package contains all from factory supported property type handlers.
-- 
--
-- NOTE :: for now the only type implemented is Unbounded_String
-- TODO :: implement support for more types.



---------------
-- Ada Works --
---------------

with Aw_Ent;


---------
-- APQ --
---------

with APQ;


package body Aw_Ent.Properties is

	-------------------------------
	-- Unbounded String property --
	-------------------------------
	procedure Set_Property(	
				Property: in     UString_Property_Type;		-- the property worker
				Entity	: in out Entity_Type'Class;		-- the entity
				Q	: in out APQ.Root_Query_Type'Class	-- the query from witch to fetch the result
			) is
		-- Set the property into the Entity.
	begin
		Property.Setter.all(
				Entity,
				To_Unbounded_String( APQ.Value( Q, To_String( Property.Name ) ) )
			);
	end Set_Property;

	procedure Get_Property(
				Property: in     UString_Property_Type;		-- the property worker
				Entity	: in out Entity_Type'Class;		-- the entity
				Query	: in out APQ.Root_Query_Type'Class	-- the query to witch append the value to insert
			) is
	begin
		APQ.Append_Quoted( Query, To_String( Property.Getter.All( Entity ) ) );
	end Get_Property;


	function New_UString_Property(
				Name	: in     String;
				Getter	: in     UString_Getter_Type;
				Setter	: in     UString_Setter_Type
			) return Entity_Property_Ptr is
		-- used to assist the creation of UString properties.
	begin
		return new UString_Property_Type'(
				Name	=> To_Unbounded_String( Name ),
				Getter	=> Getter,
				Setter	=> Setter
				);
	end New_UString_Property;


end Aw_Ent.Properties;
