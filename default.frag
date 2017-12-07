uniform sampler2D tex;

void main(void) {
    vec2 texCoord = vec2(gl_TexCoord[0]);
    vec4 color = texture2D(tex, texCoord);
    gl_FragColor = color + vec4(gl_Color.rgb, 0);
}
