import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Scanner;


public class Library {
    private final static int maximst = 5; //a student can borrow at most 5 books
    private final static int maximfa = 10; // a faculty member can borrow at most 10 books
    private int currentbooknumber;
    private StackInList<Book> booklist = new StackInList<Book>();
    private StackInList<User> usrlist = new StackInList<User>();

    //interface
    interface AboutBook {
        void borrowed();

        void returned();
    }

    interface AboutUser {
        void borrow(String bookname);

        void bback(String bookname);
    }

    //class
    class StackInList<item> implements Iterable<item> { //evaluate infix expression
        Node first = null;

        class Node {
            item item;
            Node next;
        }

        public void push(item s) {
            Node oldfirst = first;
            first = new Node();
            first.item = s;
            first.next = oldfirst;

        }

        public item pop() {
            item s = first.item;
            first = first.next;
            return s;
        }

        public boolean isEmpty() {
            return first == null;
        }

        @Override
        public Iterator<item> iterator() {
            return new stackIterator();
        }

        public class stackIterator implements Iterator<item> {
            Node cur = first;

            @Override
            public item next() {
                if (!hasNext()) {
                    throw new NoSuchElementException();
                }
                item s = cur.item;
                cur = cur.next;
                return s;
            }

            @Override
            public boolean hasNext() {
                return cur != null;
            }
        }
    }

    class User implements AboutUser {
        char type; //s stands for student; f stands for faculty
        String[] books;
        int borrowedbook;
        int maxim;
        String Id;

        User() {
        }

        User(char type, String Id) {
            this.type = type;
            this.Id = Id;
            if (type == 's') {
                maxim = maximst;
            }
            else {
                maxim = maximfa;
            }
            books = new String[maxim];
        }

        public void borrow(String bookname) {
            boolean findusr = false;
            for (User usr : usrlist) {
                if (usr.Id.equals(this.Id)) {
                    findusr = true;
                }
            }
            if (!findusr) {
                System.out.println("please sign up in the library");
                return;
            }

            if (borrowedbook == maxim) {
                System.out.println("before borrow a new book, please return a book");
                return;
            }
            boolean findbook = false;
            for (Book book : booklist) {
                if (book.name.equals(bookname) || book.number.equals(bookname)) {
                    findbook = true;
                    if (book.availableNumber > 0) {
                        if (book.number.equals(bookname)) {
                            books[borrowedbook++] = book.name;
                            book.borrowed();
                        }
                        else {
                            books[borrowedbook++] = bookname;
                            book.borrowed();
                        }
                    }
                    else {
                        System.out.println("this book is not available now");
                    }
                    break;
                }

            }
            if (!findbook) {
                System.out.println("we don't have this book currently");
            }

        }

        public void bback(String bookname) {
            boolean findbook = false;
            for (Book book : booklist) {
                if (book.name.equals(bookname) || book.number.equals(bookname)) {
                    book.returned();
                    findbook = true;
                }
            }
            if (!findbook) {
                System.out.println("please check your bookname or booknumber");
                return;
            }
            for (int i = 0; i < borrowedbook; i++) {
                if (books[i].equals(bookname)) {
                    books[i] = books[--borrowedbook];
                    books[borrowedbook] = null;
                    break;
                }
            }

        }

    }

    class Book implements AboutBook {
        String name;
        String number;
        int availableNumber;

        Book(String name, String number) {
            this.name = name;
            this.number = number;
            availableNumber = 1;
        }

        public void borrowed() {
            availableNumber--;
        }

        public void returned() {
            availableNumber++;
        }
    }

    //method
    public void shelf(String bookname, String booknumber) {
        boolean findbook = false;
        if (!booklist.isEmpty()) {
            for (Book book : booklist) {
                if (book.name.equals(bookname)) {
                    findbook = true;
                    book.availableNumber += 1;
                }
            }
        }
        if (!findbook) {
            Book newbook = new Book(bookname, booknumber);
            booklist.push(newbook);
        }
        currentbooknumber += 1;
    }

    public void shelf(String bookname) {
        boolean findbook = false;
        if (!booklist.isEmpty()) {
            for (Book book : booklist) {
                if (book.name.equals(bookname) || book.number.equals(bookname)) {
                    findbook = true;
                    book.availableNumber += 1;
                }
            }
        }
        if (!findbook) {
            System.out.println("it's a new book, please also give book number");
        }

    }

    public void usrSignUp(User usr) {
        if (usrlist.isEmpty()) {
            usrlist.push(usr);
            return;
        }
        for (User user : usrlist) {
            if (user.Id.equals(usr.Id)) {
                System.out.println("you've signed up before");
                return;
            }
        }
        usrlist.push(usr);
    }

    public int getCurrentbooknumber() {
        return currentbooknumber;
    }

    public void queryUser(String Id) {
        if (usrlist.isEmpty()) {
            System.out.println("there is no such an user");
        }
        for (User user : usrlist) {
            if (user.Id.equals(Id)) {
                System.out.println("Id: " + user.Id);
                System.out.println("Type: " + user.type);
                System.out.println("book: " + user.borrowedbook + "/" + user.maxim);
                return;
            }
        }
        System.out.println("there is no such an user");
    }

    public void queryBook(String book) {
        if (booklist.isEmpty()) {
            System.out.println("there is no such a book");
        }
        for (Book bok : booklist) {
            if (bok.name.equals(book) || bok.number.equals(book)) {
                System.out.println("name: " + bok.name);
                System.out.println("number: " + bok.number);
                System.out.println("availableNumber: " + bok.availableNumber);
                return;
            }
        }
        System.out.println("there is no such a book");
    }

    public static void main(String[] args) {
        Library mylibrary = new Library();
        System.out.println("input books you want to shelf, end with '#'");
        Scanner input = new Scanner(System.in);
        String str = input.nextLine();
        String[] bookinfo;
        while (!str.equals("#")) {

            bookinfo = str.split(",");
            if (bookinfo.length == 1) mylibrary.shelf(bookinfo[0]);
            else mylibrary.shelf(bookinfo[0], bookinfo[1]);

            str = input.nextLine();
        }
        System.out.println(
                "use bookname or book number to query information about the book in the library,end query with#");
        str = input.nextLine();
        while (!str.equals("#")) {
            mylibrary.queryBook(str);
            str = input.nextLine();
        }
        System.out.println("start establish user");
        str = input.nextLine();

        Library.StackInList<Library.User> allusers = mylibrary.new StackInList<Library.User>();
        while (!str.equals("#")) {
            bookinfo = str.split(",");


            Library.User thisuser = mylibrary.new User(bookinfo[0].toCharArray()[0], bookinfo[1]);
            System.out.println("do you want this user to sign up in your library?");

            str = input.nextLine();
            if (str.equals("y")) {
                mylibrary.usrSignUp(thisuser);
            }
            allusers.push(thisuser);
            str = input.nextLine();
        }
        Library.User thisuser;
        System.out.println(
                "start borrowbook, what book do you want to borrow(try a not signed up user");
        str = input.nextLine();
        thisuser = allusers.pop();
        thisuser.borrow(str);
        System.out.println(
                "start borrowbook, what book do you want to borrow(try a not existing book");
        str = input.nextLine();
        thisuser = allusers.pop();
        thisuser.borrow(str);
        System.out.println("start borrowbook, what book do you want to borrow(try a good book");
        str = input.nextLine();
        thisuser.borrow(str);
        mylibrary.queryUser(thisuser.Id);
        mylibrary.queryBook(str);
        input.close();
    }

}
