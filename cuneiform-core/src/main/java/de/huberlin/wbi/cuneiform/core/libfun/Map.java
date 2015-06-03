package de.huberlin.wbi.cuneiform.core.libfun;

import java.util.HashMap;
import java.util.Set;

public class Map implements Term {

	private final HashMap<Term,Term> content;
	
	public Map( Term key, Term value ) {
		this();
		
		if( key == null )
			throw new IllegalArgumentException( "Key term must not be null." );
		
		if( key instanceof Placeholder )
			throw new PhAsKeyInTermMapException();
		
		if( value == null )
			throw new IllegalArgumentException( "Value term must not be null." );
		
		content.put( key, value );
	}
	
	public Map() {
		content = new HashMap<>();
	}
	
	private Map( HashMap<Term,Term> content ) {
		this.content = content;
	}
	
	public Map put( Term key, Term value ) {
		
		HashMap<Term,Term> newContent;
		
		if( key == null )
			throw new IllegalArgumentException( "Key term must not be null." );
		
		if( key instanceof Placeholder )
			throw new PhAsKeyInTermMapException();
		
		if( value == null )
			throw new IllegalArgumentException( "Value term must not be null." );
		
		newContent = new HashMap<>();
		newContent.putAll( content );
		newContent.put( key, value );
		
		return new Map( newContent );
	}
	
	public Term get( Term key ) {
		
		Term value;
		
		if( key == null )
			throw new IllegalArgumentException( "Key must not be null." );
		
		if( key instanceof Placeholder )
			throw new PhAsKeyInTermMapException();
		
		value = content.get( key );
		if( value == null )
			throw new UnboundKeyException( key );
		
		return value;
	}
	
	public Term get( Term key, Term def ) {
		
		if( key == null )
			throw new IllegalArgumentException( "Key term must not be null." );
		
		if( key instanceof Placeholder )
			throw new PhAsKeyInTermMapException();
		
		if( def == null )
			throw new IllegalArgumentException( "Default term must not be null." );
		
		if( content.containsKey( key ) )
			return content.get( key );
		
		return def;
		
	}
	
	public Map merge( Map tm2 ) {
		
		HashMap<Term,Term> newContent;
		
		if( tm2 == null )
			throw new IllegalArgumentException( "Other term map must not be null." );
		
		newContent = new HashMap<>();
		newContent.putAll( tm2.content );
		newContent.putAll( content );
		
		return new Map( newContent );		
	}
	
	public int size() {
		return content.size();
	}
	
	public boolean isEmpty() {
		return content.isEmpty();
	}

	@Override
	public boolean unify( Term other ) {
		
		Map tm2;
		
		if( other == null )
			throw new IllegalArgumentException( "Other term must not be null." );
		
		if( other instanceof Placeholder )
			throw new PhOnRightHandSideException();
		
		if( !( other instanceof Map ) )
			return false;
		
		tm2 = ( Map )other;
		
		if( content.size() != tm2.size() )
			return false;
		
		for( Term key : keys() ) {
			
			if( !tm2.isKey( key ) )
				return false;
			
			if( !get( key ).unify( tm2.get( key ) ) )
				return false;
		}
		
		return true;
	}

	private Set<Term> keys() {
		return content.keySet();
	}

	@Override
	public String print() {
		
		StringBuffer buf;
		boolean comma;
		
		buf = new StringBuffer();
		
		buf.append( "#{" );
		
		comma = false;
		for( Term key : content.keySet() ) {
			
			if( comma )
				buf.append( ',' );
			comma = true;
			
			buf.append( key ).append( "=>" ).append( content.get( key ) );
		}
		
		buf.append( '}' );
		
		return buf.toString();
	}

	@Override
	public void unspecialize() {
		
		for( Term value : content.values() )
			value.unspecialize();
	}

	public boolean isKey( Term key ) {
		return content.containsKey( key );
	}
}