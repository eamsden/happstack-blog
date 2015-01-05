# A Happstack Blog Engine

I've written a simple blog engine to tie into my personal site, which uses
[Happstack](http://www.happstack.com). The blog engine uses Happstack, along
with the [markdown](http://hackage.haskell.org/package/markdown) 
Haskell library to render posts from Markdown, and the
[cassava](http://hackage.haskell.org/package/cassava) library for parsing a
CSV database of posts. I'm also working on adding RSS feed support, but the
available Haskell libraries for this are a bit more tricky.

Using:

Serving just a blog:

    {-# LANGUAGE OverloadedStrings #-}
    import Happstack.Blog
    import Happstack.Lite
    
    main :: IO ()
    main = serve (Just $ defaultServerConfig { port = 8080 }) $ blog "Tea and Time" "blog"

Serving a blog as a subdirectory:

    {-# LANGUAGE OverloadedStrings #-}
    import Happstack.Blog
    import Happstack.Lite
    
    main :: IO ()
    main = serve (Just $ defaultServerConfig { port = 8080 }) site

    site :: ServerPart Response
    site = msum [ dir "blog" $ blog "Tea and Time" "blog", ok $ toResponse "I'm OK"] 

In both these cases, the file `entries.csv` should appear in the subdirectory "blog"
of the working directory of the server. The format of entries.csv is:

    "Post title 1","2015-01-05 13:01:00","post-title-1"
    "I like cats","2015-01-05 14:02:00","i-like-cats

The first field is the title of the post, the second is its timestamp, and the third is both the url slug and the
name of the markdown file, minus the .md. So assuming that `post-title-1.md` is in the `blog` subdirectory along with
the entries.csv file, the post should be shown at `<blog-url>/post-title-1`.

## Architecture
Currently the blog engine is a single module which exports a single function.
When tied into Happstack, the `blog` function will read the "entries.csv"
file in the given directory. Each row is a post. The first column is titles,
the second is post dates, and the third is names. The name is used both as 
the filename for the post content (in the same directory as entries.csv,
and with the .md extension added to the name). `blog` also takes the blog
title as a parameter.

## Still To Do
Currently I'm working on:

* RSS feeds

