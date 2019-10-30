var gulp = require("gulp");
var path = require("path");

require("./build/gulp_task/create_inner_file/shaderChunk");

gulp.task("generateIndex", function (done) {
    var generate = require("wonder-generate-index");
    var rootDir = path.join(process.cwd(), "src"),
        destDir = "./src/";

    generate.generate("/", rootDir, ["**/api/js/**/*.re"], destDir, {
        exclude: []
    });

    done();
});
