with APQ;


package body KOW_ENT.Properties.Generic_Decimal is


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


	function Get_Precision( P : in Val_Type ) return Integer is
	begin
		if P > 0.1 then
			return 0;
		elsif P = 0.1 then
			return 1;
		else
			return 1 + Get_Precision( P => P * 10.0 );
		end if;
	end Get_Precision;

	overriding
	procedure Append_Create_Table(
				Property	: in     Property_Type;
				Query		: in out APQ.Root_Query_Type'Class
			) is
	begin
		APQ.Append(
				Query,
				To_String( Property.Column_Name ) &
					" DECIMAL(" &
						Integer'Image( Val_Type'Digits ) &
						"," &
						Integer'Image( Get_Precision( Val_Type'Delta ) ) &
					") NOT NULL"
			);

	end Append_Create_Table;




	function New_Property(
					Column_Name	: in String;
					Getter		: not null access function( Entity : in Entity_Type'Class ) return Val_Type;
					Setter		: not null access procedure( Entity : in out Entity_Type'Class; Value : in Val_Type );
					Immutable	: in Boolean := False
			) return KOW_Ent.Entity_Property_Ptr is
		PT : Property_Type;
	begin
		PT.Column_name	:= To_Unbounded_String( Column_name );
		PT.Getter	:= Getter;
		PT.Setter	:= Setter;
		PT.Immutable	:= Immutable;
		return new Property_Type'( PT );
	end New_Property;



end KOW_Ent.Properties.Generic_Decimal;
