



--------------
-- Ada 2005 --
--------------

with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;
with Ada.Text_IO;		use Ada.Text_IO;


---------
-- APQ --
---------

with APQ;
with APQ_Provider;
with APQ_Provider.MySQL;
pragma Elaborate( APQ_Provider.MySQL );


------------
-- Aw_Ent --
------------

with Aw_Ent;		use Aw_Ent;
with Aw_Config;
with Aw_Config.Text;

-----------------
-- My Packages --
-----------------

with Book_Entities;



procedure Book_Store is
	A_Book : Book_Entities.Book_Type;
	B_Book : Book_Entities.Book_Type;



	Provider_Config : Aw_Config.Config_File;
	Provider	: APQ_Provider.Connection_Provider_Ptr := new APQ_Provider.Connection_Provider_Type;
begin

	Aw_Config.Set_Project_Name( "BOOK_STORE" );
	
	Provider_Config := Aw_Config.New_Config_File( "aw_ent", new Aw_Config.Text.Parser );
	Provider.Setup( Provider_Config );


	Aw_Ent.Set_Connection_Provider( Provider );


	--------------
	-- Entity A --
	--------------

	Put_Line( "Defining book A" );
	Book_Entities.Set_Title(
			Book		=> Entity_Type'Class( A_Book ),
			Title		=> To_Unbounded_String( "Book of the real funny thing" )
		);
	Book_Entities.Set_Extra_Info(
			Book		=> Entity_Type'Class( A_Book ),
			Extra_Info	=> To_Unbounded_String( "This book is about another impressive thing on this word: the possibility of the impossible!" )
		);

	Put_Line( "Storing book A" );
	Store( A_Book );

	Put_Line( "Putting book A" );
	Book_Entities.Put( A_Book );


	New_Line( 3 );



	--------------
	-- Entity B --
	--------------

	Put_Line( "Loading book B :: id == 2" );
	Load( B_Book, 2 );

	Put_Line( "Putting book B");
	Book_Entities.Put( B_Book );


	Book_Entities.Set_Title(
			Book	=> Entity_Type'Class( B_Book ),
			Title	=> Book_Entities.Get_Title( Entity_Type'Class( B_Book ) ) & To_Unbounded_String( "=" )
		);


	New_Line;
	Put_Line( "Saving book B back into the database" );
	Aw_Ent.Store( B_Book );



	-------------------------------
	-- Now we Query the Database --
	-------------------------------

	declare
		Q	: Book_Entities.Book_Query.Query_Type;


		procedure Put_All( C : Book_Entities.Book_Query.Entity_Vectors.Cursor ) is
		begin
			Book_Entities.Put( Book_Entities.Book_Query.Entity_Vectors.Element( C ) );
		end Put_All;
	begin

		New_Line( 3 );
		Put_Line( "Results for some values" );
		New_Line( 3 );
		 
		Book_Entities.Book_Query.Append( 
				Q		=> Q,
				Column		=> "title",
				Value		=> "%=%",
				Appender	=> Book_Entities.Book_Query.Appender_AND,
				Operator	=> Book_Entities.Book_Query.Operator_Like
			);

		Book_Entities.Book_Query.Append( 
				Q		=> Q,
				Column		=> "title",
				Value		=> "%L%",
				Appender	=> Book_Entities.Book_Query.Appender_And,
				Operator	=> Book_Entities.Book_Query.Operator_Like
				
			);



		Book_Entities.Book_Query.Entity_Vectors.Iterate(
				Book_Entities.BOok_Query.Get_All( Q ),
				Put_All'Access
			);

		New_Line( 3 );
		Put_Line("And now again the first result" );
		New_Line( 3 );
		Book_Entities.Put(
				Book_Entities.Book_Query.Get_First(
						Q	=> Q,
						Unique	=> False
					)
				);

		
	end;

	--
	-- Testing the relations ::
	--
	
	declare
		use Book_Entities;
		use Book_Entities.Author_Books_Handlers;
		Author : Author_Type;
		Books  : Related_Entity_Query_Builders.Entity_Vectors.Vector;

		procedure Put_All( C : Related_Entity_Query_Builders.Entity_Vectors.Cursor ) is
			Book : Book_Type := Related_Entity_Query_Builders.Entity_Vectors.Element( C );
		begin
			Book_Entities.Put( Book );
			Book.Extra_Info := Book.Extra_Info & To_Unbounded_String( "|" );

			Related_Entity_Query_Builders.Entity_Vectors.Replace_Element( Books, C, Book );
		end Put_All;
	begin
		Aw_Ent.Load( Author, 2 );

		Ada.Text_IO.New_Line( 3 );
		Ada.Text_IO.Put_Line( "=======================================" );
		Ada.Text_IO.Put_Line( "Getting books from """ & To_String( Author.Name ) & """" );
		Ada.Text_IO.Put_Line( "=======================================" );
		Books := Get_All( Author );
		Related_Entity_Query_Builders.Entity_Vectors.Iterate(
				Books,
				Put_All'Access
			);
		Store_All( Author, Books );
		Ada.Text_IO.Put_Line( "=======================================" );
		Ada.Text_IO.New_Line( 3 );
	end;

end Book_Store;

