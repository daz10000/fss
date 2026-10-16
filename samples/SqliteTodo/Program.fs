// fss_sqlite: a tiny command-line to-do list stored in a SQLite file.
//
//   dotnet run --project samples/SqliteTodo -- add "buy milk"
//   dotnet run --project samples/SqliteTodo -- list
//   dotnet run --project samples/SqliteTodo -- done 1
//   dotnet run --project samples/SqliteTodo            (no args: runs a scripted demo)
//
// Set TODO_DB to choose the database file (default: todo.db in the temp folder).

open System
open System.IO
open Fss.Data.SQLite

// SQLite has few native types: integers come back as int64, booleans are stored as 0/1
// and dates as text. fss converts to the record's field types, so `bool` and `int` are fine.
type Todo = { id : int64 ; title : string ; completed : bool ; created : string ; due : string option }

/// Shape used for inserts: no id (SQLite assigns it)
type NewTodo = { title : string ; completed : bool ; created : string ; due : string option }

let schema = """
create table if not exists todo (
    id      integer primary key,
    title   text not null,
    completed integer not null default 0,
    created text not null,
    due     text
)"""

let openDb () =
    let path =
        match Environment.GetEnvironmentVariable "TODO_DB" with
        | null | "" -> Path.Combine(Path.GetTempPath(), "fss-todo.db")
        | p -> p
    let db = new DynamicSqlConnection(sprintf "Data Source=%s" path)
    db.ExecuteNonQuery schema |> ignore
    db

let add (db:DynamicSqlConnection) title due =
    // SQLite returns the new rowid (int64) via last_insert_rowid()
    let id : int64 =
        db.InsertOne({ title = title ; completed = false ; created = DateTime.UtcNow.ToString "s" ; due = due }, table = "todo")
    printfn "added #%d %s" id title
    id

let list (db:DynamicSqlConnection) =
    let todos = db.Query<Todo> "select * from todo order by completed, id" |> List.ofSeq
    if todos.IsEmpty then printfn "(nothing to do)"
    for t in todos do
        let due = t.due |> Option.map (sprintf " (due %s)") |> Option.defaultValue ""
        printfn "  [%s] #%d %s%s" (if t.completed then "x" else " ") t.id t.title due
    todos

let complete (db:DynamicSqlConnection) (id:int64) =
    // SQLite parameters can be written @name, :name or $name
    match db.ExecuteNonQuery("update todo set completed = 1 where id = @id", [ "id", box id ]) with
    | 0 -> printfn "no to-do #%d" id
    | _ -> printfn "completed #%d" id

[<EntryPoint>]
let main argv =
    use db = openDb ()
    match argv |> List.ofArray with
    | [ "add" ; title ] -> add db title None |> ignore ; 0
    | [ "add" ; title ; due ] -> add db title (Some due) |> ignore ; 0
    | [ "list" ] -> list db |> ignore ; 0
    | [ "done" ; id ] -> complete db (int64 id) ; 0
    | [] ->
        // Scripted demo in a throwaway database
        db.ExecuteNonQuery "delete from todo" |> ignore
        let a = add db "write the docs" (Some "2026-11-01")
        add db "publish to NuGet" None |> ignore
        add db "celebrate" None |> ignore
        complete db a
        let todos = list db
        if todos.Length <> 3 || not (todos |> List.exists (fun t -> t.id = a && t.completed)) then failwith "unexpected state"
        0
    | _ ->
        eprintfn "usage: SqliteTodo [add <title> [due] | list | done <id>]"
        1
