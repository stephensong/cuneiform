package de.huberlin.wbi.cuneiform.core.funsem;

import java.util.HashMap;
import java.util.Set;

public class ImmutableMap<K,V> {

	private final HashMap<K,V> content;
	
	public ImmutableMap( K key, V value ) {
		this();
		
		if( key == null )
			throw new IllegalArgumentException( "Key term must not be null." );
		
		if( value == null )
			throw new IllegalArgumentException( "Value term must not be null." );
		
		content.put( key, value );
	}
	
	public ImmutableMap() {
		content = new HashMap<>();
	}
	
	private ImmutableMap( HashMap<K,V> content ) {
		this.content = content;
	}
	
	public ImmutableMap<K,V> put( K key, V value ) {
		
		HashMap<K,V> newContent;
		
		if( key == null )
			throw new IllegalArgumentException( "Key term must not be null." );
		
		if( value == null )
			throw new IllegalArgumentException( "Value term must not be null." );
		
		newContent = new HashMap<>();
		newContent.putAll( content );
		newContent.put( key, value );
		
		return new ImmutableMap<>( newContent );
	}
	
	public V get( K key ) {
		
		V value;
		
		if( key == null )
			throw new IllegalArgumentException( "Key must not be null." );
				
		value = content.get( key );
		
		if( value == null )
			throw new RuntimeException( "The key "+key+" is unbound." );
		
		return value;
	}
	
	public V get( K key, V def ) {
		
		if( key == null )
			throw new IllegalArgumentException( "Key term must not be null." );
		
		if( def == null )
			throw new IllegalArgumentException( "Default term must not be null." );
		
		if( content.containsKey( key ) )
			return content.get( key );
		
		return def;
	}
	
	public ImmutableMap<K,V> merge( ImmutableMap<K,V> tm2 ) {
		
		HashMap<K,V> newContent;
		
		if( tm2 == null )
			throw new IllegalArgumentException( "Other term map must not be null." );
		
		newContent = new HashMap<>();
		newContent.putAll( content );
		newContent.putAll( tm2.content );
		
		return new ImmutableMap<>( newContent );		
	}
	
	public int size() {
		return content.size();
	}
	
	public boolean isEmpty() {
		return content.isEmpty();
	}

	public Set<K> keys() {
		return content.keySet();
	}

	@Override
	public String toString() {
		
		StringBuffer buf;
		boolean comma;
		
		buf = new StringBuffer();
		
		buf.append( "#{" );
		
		comma = false;
		for( K key : content.keySet() ) {
			
			if( comma )
				buf.append( ',' );
			comma = true;
			
			buf.append( key ).append( "=>" ).append( content.get( key ) );
		}
		
		buf.append( '}' );
		
		return buf.toString();
	}

	public boolean isKey( K key ) {
		return content.containsKey( key );
	}

}
