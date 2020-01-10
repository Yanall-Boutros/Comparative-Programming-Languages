// $Id: queue.java,v 1.2 2011-12-15 18:15:36-08 - - $

class queue<item_t> {
   static class emptyexn extends RuntimeException {};

   class node {
      item_t item;
      node link;
   };

   node head = null;
   node last = null;

   boolean empty () {
      return head == null;
   };

   void insert (item_t item) {
      node newnode = new node ();
      newnode.item = item;
      if (head == null) head = newnode;
                   else last.link = newnode;
      last = newnode;
   };

   item_t remove () {
      if (empty ()) throw new emptyexn ();
      item_t result = head.item;
      head = head.link;
      if (head == null) last = null;
      return result;
   };

};
