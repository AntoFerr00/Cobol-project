       IDENTIFICATION DIVISION.
       PROGRAM-ID. SHOP-SYSTEM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SHOP-FILE ASSIGN TO "shopfile.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TEMP-FILE ASSIGN TO "tempfile.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD SHOP-FILE.
       01 SHOP-ITEM.
           05 ITEM-ID PIC 9(5).
           05 ITEM-NAME PIC X(30).
           05 ITEM-PRICE PIC 9(5)V99.
           05 ITEM-STOCK PIC 9(5).

       FD TEMP-FILE.
       01 TEMP-ITEM.
           05 TEMP-ID PIC 9(5).
           05 TEMP-NAME PIC X(30).
           05 TEMP-PRICE PIC 9(5)V99.
           05 TEMP-STOCK PIC 9(5).

       WORKING-STORAGE SECTION.
       01 USER-CHOICE PIC 9.
       01 ITEM-ID-INPUT PIC 9(5).
       01 ITEM-NAME-INPUT PIC X(30).
       01 ITEM-PRICE-INPUT PIC 9(5)V99.
       01 ITEM-STOCK-INPUT PIC 9(5).
       01 ITEM-REMOVE-ID PIC 9(5).
       01 END-OF-FILE PIC X VALUE 'N'.
       01 OS-COMMAND PIC X(50).

       PROCEDURE DIVISION.
       MAIN-LOOP.
           DISPLAY "SHOP MANAGEMENT SYSTEM".
           DISPLAY "1. Add Item".
           DISPLAY "2. View Items".
           DISPLAY "3. Remove Item".
           DISPLAY "4. Exit".
           DISPLAY "Enter your choice: ".
           ACCEPT USER-CHOICE.

           EVALUATE USER-CHOICE
               WHEN 1
                   PERFORM ADD-ITEM
               WHEN 2
                   PERFORM VIEW-ITEMS
               WHEN 3
                   PERFORM REMOVE-ITEM
               WHEN 4
                   DISPLAY "Exiting..."
                   STOP RUN
               WHEN OTHER
                   DISPLAY "Invalid choice, try again."
           END-EVALUATE.

           GO TO MAIN-LOOP.

       ADD-ITEM.
           DISPLAY "Enter Item ID: ".
           ACCEPT ITEM-ID-INPUT.
           DISPLAY "Enter Item Name: ".
           ACCEPT ITEM-NAME-INPUT.
           DISPLAY "Enter Item Price: ".
           ACCEPT ITEM-PRICE-INPUT.
           DISPLAY "Enter Item Stock: ".
           ACCEPT ITEM-STOCK-INPUT.

           OPEN EXTEND SHOP-FILE.
           MOVE ITEM-ID-INPUT TO ITEM-ID.
           MOVE ITEM-NAME-INPUT TO ITEM-NAME.
           MOVE ITEM-PRICE-INPUT TO ITEM-PRICE.
           MOVE ITEM-STOCK-INPUT TO ITEM-STOCK.
           WRITE SHOP-ITEM.
           CLOSE SHOP-FILE.

           DISPLAY "Item added successfully!".

       VIEW-ITEMS.
           OPEN INPUT SHOP-FILE.
           DISPLAY "Item List:".
           MOVE 'N' TO END-OF-FILE.

           PERFORM UNTIL END-OF-FILE = 'Y'
               READ SHOP-FILE AT END
                   MOVE 'Y' TO END-OF-FILE
               NOT AT END
                   DISPLAY "ID: " ITEM-ID " Name: " ITEM-NAME " Price: " ITEM-PRICE " Stock: " ITEM-STOCK
               END-READ
           END-PERFORM.

           CLOSE SHOP-FILE.
           DISPLAY "End of list.".

       REMOVE-ITEM.
           DISPLAY "Enter the Item ID to remove: ".
           ACCEPT ITEM-REMOVE-ID.

           OPEN INPUT SHOP-FILE.
           OPEN OUTPUT TEMP-FILE.
           MOVE 'N' TO END-OF-FILE.

           PERFORM UNTIL END-OF-FILE = 'Y'
               READ SHOP-FILE AT END
                   MOVE 'Y' TO END-OF-FILE
               NOT AT END
                   IF ITEM-ID NOT = ITEM-REMOVE-ID
                       MOVE ITEM-ID TO TEMP-ID
                       MOVE ITEM-NAME TO TEMP-NAME
                       MOVE ITEM-PRICE TO TEMP-PRICE
                       MOVE ITEM-STOCK TO TEMP-STOCK
                       WRITE TEMP-ITEM
                   ELSE
                       DISPLAY "Item removed successfully."
                   END-IF
               END-READ
           END-PERFORM.

           CLOSE SHOP-FILE.
           CLOSE TEMP-FILE.

           DISPLAY "Updating item list...".
           CALL "SYSTEM" USING "IF EXIST shopfile.dat DEL shopfile.dat".
           CALL "SYSTEM" USING "RENAME tempfile.dat shopfile.dat".

           DISPLAY "Updated item list.".
