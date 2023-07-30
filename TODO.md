
The implicit starting point, and relative positioning positibility mean
  any bounds checks require the whole path
  or an accompanying starting position for each word

  Maybe this is simply a parameter to the bounding_box method
    Supplied as needed, including while running through a loop





Define Vec types so that I can call scale on them?

Need to center path before scaling, then move back
Centering path requires knowing bounds of curves....

To find the bounding box of the entire svg
  Accumulate all bounding boxes of all SvgWords
  This requires iterating through the path,
    Accumulating current position from endpoints
      Bounding box calculations will be from current position

rust bezier lib -> https://github.com/dorianprill/stroke
bounds -> https://github.com/nfroidure/svg-pathdata/blob/dcdd9e361af9829acfadd0d221850806752e535f/src/SVGPathData.ts#L23
