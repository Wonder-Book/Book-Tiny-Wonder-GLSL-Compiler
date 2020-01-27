let _getFunctionContent = () => {|
  open ShaderChunkType;

  open StateDataType;

  let _getGLSLChunkMap = ({chunkMap}) => chunkMap;

  let getChunk = (name: string, glslChunkData) =>
    glslChunkData
    |> _getGLSLChunkMap
    |> TinyWonderCommonlib.ImmutableHashMap.get(name)
    |> Js.Option.getExn;

  let _buildChunk =
      (
        (top: string, define: string),
        varDeclare: string,
        (funcDeclare: string, funcDefine: string),
        body: string
      ) => {
    top,
    define,
    varDeclare,
    funcDeclare,
    funcDefine,
    body
  };

  let create = () =>
  |};

let _buildInitDataContent = (glslContent: string) => {j|
    TinyWonderCommonlib.ImmutableHashMap.{
      chunkMap:
        createEmpty()
        $glslContent
    };
  |j};

let _buildShaderChunkFileContent = glslContent =>
  _getFunctionContent() ++ _buildInitDataContent(glslContent);

let _writeToShaderChunkFile = (destFilePath, doneFunc, content) => {
  Node.Fs.writeFileSync(destFilePath, content, `utf8);
  doneFunc(.) |> ignore;
};

let _convertArrayToList = (array: array(string)) =>
  array |> Js.Array.reduce((list, str) => [str, ...list], []);

let createShaderChunkFile =
    (glslPathArr: array(string), destFilePath: string, doneFunc) =>
  glslPathArr
  |> Js.Array.map(glslPath => Glob.sync(glslPath))
  |> ArrayUtils.flatten
  |> _convertArrayToList
  |> List.map(actualGlslPath =>
       Node.Fs.readFileSync(actualGlslPath, `utf8)
       |> Parse.parseSegment(actualGlslPath)
     )
  |> Parse.parseImport
  |> _buildShaderChunkFileContent
  |> _writeToShaderChunkFile(destFilePath, doneFunc);