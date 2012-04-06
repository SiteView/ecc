package com.dragonflow.siteview.exchange;

import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

public class MultiValueHashMap <K, V> extends HashMap<K, Collection<V>>
   implements Map<K, Collection<V>>
 {
   public void putValue(K key, V value)
   {
     Collection vs = (Collection)get(key);
     if (vs == null) {
       vs = new LinkedList();
       put(key, vs);
     }
 
     vs.add(value);
   }
 
   public Collection<V> getValues(K key)
   {
     return ((Collection)get(key));
   }
}
