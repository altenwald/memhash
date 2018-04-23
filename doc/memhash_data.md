

# Module memhash_data #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-table_id">table_id()</a> ###


__abstract datatype__: `table_id()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add-1">add/1</a></td><td></td></tr><tr><td valign="top"><a href="#decr-1">decr/1</a></td><td></td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_links-1">get_links/1</a></td><td></td></tr><tr><td valign="top"><a href="#incr-1">incr/1</a></td><td></td></tr><tr><td valign="top"><a href="#set-2">set/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add-1"></a>

### add/1 ###

<pre><code>
add(Data::term()) -&gt; <a href="#type-table_id">table_id()</a>
</code></pre>
<br />

<a name="decr-1"></a>

### decr/1 ###

<pre><code>
decr(ID::<a href="#type-table_id">table_id()</a>) -&gt; ok
</code></pre>
<br />

<a name="get-1"></a>

### get/1 ###

<pre><code>
get(ID::<a href="#type-table_id">table_id()</a>) -&gt; term()
</code></pre>
<br />

<a name="get_links-1"></a>

### get_links/1 ###

<pre><code>
get_links(ID::<a href="#type-table_id">table_id()</a>) -&gt; pos_integer()
</code></pre>
<br />

<a name="incr-1"></a>

### incr/1 ###

<pre><code>
incr(ID::<a href="#type-table_id">table_id()</a>) -&gt; ok
</code></pre>
<br />

<a name="set-2"></a>

### set/2 ###

<pre><code>
set(ID::<a href="#type-table_id">table_id()</a>, Data::term()) -&gt; ok
</code></pre>
<br />

<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

<a name="stop-0"></a>

### stop/0 ###

`stop() -> any()`

