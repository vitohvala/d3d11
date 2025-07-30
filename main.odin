package win32_project0

import "core:fmt"
import win "core:sys/windows"
import "core:slice"
import "core:mem"
import "core:mem/virtual"
import "core:os"
import "core:strings"
import "base:runtime"
import "core:log"
import "core:time"
import dx11 "vendor:directx/d3d11"
import dxgi "vendor:directx/dxgi"
import d3d  "vendor:directx/d3d_compiler"
import stbi "vendor:stb/image"
import "core:math"
import "core:math/linalg"



Color :: [3]f32
vec3  :: [3]f32
vec4  :: [4]f32
vec2  :: [2]f32

Constants :: struct #align(16) {
    transform : matrix[4, 4]f32,
}

Vertex :: struct {
    pos   : vec3,
    uv    : vec2,
    color : Color,
}

VertexBatch :: struct {
    vertex : [MAX_VERTICES]Vertex,
    size : u32,
}

Renderer :: struct {
    device : ^dx11.IDevice,
    dcontext : ^dx11.IDeviceContext,
    swap_chain : ^dxgi.ISwapChain,
    rview : ^dx11.IRenderTargetView,
    vbatch : ^VertexBatch,
    vbuffer : ^dx11.IBuffer,
    vs : ^dx11.IVertexShader,
    ps : ^dx11.IPixelShader,
    input_layout : ^dx11.IInputLayout,
    rstate : ^dx11.IRasterizerState,
    cmap : ^dx11.IShaderResourceView,
    cmap_sampler : ^dx11.ISamplerState,
}

ButtonState :: struct {
    half_transition_count: int,
    ended_down: bool,
}

Buttons :: enum {
    Move_Up,
    Move_Down,
    Move_Left,
    Move_Right,
    Action_Up,
    Action_Down,
    Action_Left,
    Action_Right,
    Left_Shoulder,
    Right_Shoulder,
    Start,
    Back,
}

RED :: Color{1, 0, 0}
WHITE :: Color{1, 1, 1}
BLACK :: Color{0, 0, 0}
BLUE :: Color{0, 0, 1}
GREEN :: Color{0, 1, 0}
MAX_QUADS :: 8192
MAX_VERTICES :: MAX_QUADS * 4

WORLD_WIDTH :: 27
WORLD_HEIGHT :: 16

world := []f32{
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
}

process_keyboard_message :: proc(new_state: ^ButtonState, is_down: bool) {
    if new_state.ended_down != is_down {
        new_state.ended_down = is_down
        new_state.half_transition_count += 1
    }
}

was_pressed :: proc(state : ^ButtonState) -> bool {
	result  : bool = ((state.half_transition_count > 1) ||
	                 ((state.half_transition_count == 1) &&
	                  (state.ended_down)))
	return result
}

win_proc :: proc "stdcall" (hwnd: win.HWND,
    msg: win.UINT,
    wparam: win.WPARAM,
    lparam: win.LPARAM) -> win.LRESULT
{
    context = runtime.default_context()
    switch msg {
        case win.WM_CLOSE :
        fallthrough
        case win.WM_DESTROY :
        win.PostQuitMessage(0)
        return 0
        case win.WM_ERASEBKGND : return 1
        case win.WM_SIZE : {
            crect :win.RECT
            win.GetClientRect(hwnd, &crect)
        }
        case win.WM_NCLBUTTONDOWN:
        {
            win.SendMessageW(hwnd, win.WM_NCHITTEST, wparam, lparam)
            point : win.POINT
            win.GetCursorPos(&point)
            win.ScreenToClient(hwnd, &point)
            win.PostMessageW(hwnd, win.WM_MOUSEMOVE, 0, int(point.x | point.y << 16))
        }
        case win.WM_ENTERSIZEMOVE : { win.SetTimer(hwnd, 1, 0, nil); return 0}
        case win.WM_EXITSIZEMOVE : {win.KillTimer(hwnd, 1); return 0 }
        case win.WM_TIMER : {

            return 0
        }
        case win.WM_PAINT:
        {
            pst : win.PAINTSTRUCT
            win.BeginPaint(hwnd, &pst)

            win.EndPaint(hwnd, &pst)
        }
        case win.WM_SYSKEYDOWN :
        case win.WM_SYSKEYUP :
        case win.WM_KEYDOWN :
        case win.WM_KEYUP : {
            //
            vk_code := u32(wparam)
            if(vk_code == win.VK_F4){
                //nemoj = ~nemoj
            }
        }

    }

    return win.DefWindowProcW(hwnd, msg, wparam, lparam)
}

kilobytes :: proc(value: $T) -> T {
    return (value) * 1024
}

megabytes :: proc(value: $T) -> T {
    return kilobytes(value) * 1024
}

d3d_clean :: proc(renderer : ^Renderer) {
    renderer.device->Release()
    renderer.dcontext->Release()
    renderer.swap_chain->Release()
    renderer.rview->Release()
    renderer.vbuffer->Release()
    renderer.vs->Release()
    renderer.ps->Release()
    renderer.input_layout->Release()
    renderer.rstate->Release()
    renderer.cmap->Release()
    renderer.cmap_sampler->Release()
}

vappend :: proc(vb : ^VertexBatch, pos: vec3, uv : vec2, color : Color) {
    if vb.size < MAX_VERTICES {
        vb.vertex[vb.size] = {pos, uv, color}
        vb.size += 1
    }
}

create_window :: proc(width, height : i32) -> win.HWND {
    instance := win.HINSTANCE(win.GetModuleHandleW(nil))
    assert(instance != nil)

    wca : win.WNDCLASSW
    wca.hInstance = instance
    wca.lpszClassName = win.L("Project0")
    wca.style = win.CS_HREDRAW | win.CS_VREDRAW | win.CS_OWNDC
    wca.lpfnWndProc = win_proc

    cls := win.RegisterClassW(&wca)
    assert(cls != 0, "Class creation failed")

    wrect := win.RECT{0, 0, width, height}
    win.AdjustWindowRect(&wrect, win.WS_OVERLAPPEDWINDOW, win.FALSE)

    handle := win.CreateWindowExW(0, wca.lpszClassName,
        win.L("Program"),
        win.WS_VISIBLE | win.WS_OVERLAPPEDWINDOW,
        10, 10,
        wrect.right - wrect.left, wrect.bottom - wrect.top,
        nil, nil, instance, nil)

    assert(handle != nil)

    return handle
}

d3d_init :: proc (handle : win.HWND, arena : ^virtual.Arena) -> Renderer {
    driver_types := []dx11.DRIVER_TYPE{.HARDWARE, .WARP}
    feature_levels := []dx11.FEATURE_LEVEL{ ._11_0, ._10_1, ._10_0}

    scd : dxgi.SWAP_CHAIN_DESC
    scd.BufferCount = 1
    scd.BufferDesc.Width = 0
    scd.BufferDesc.Height = 0
    scd.BufferDesc.Format = .R8G8B8A8_UNORM
    scd.BufferDesc.RefreshRate.Numerator = 60
    scd.BufferDesc.RefreshRate.Denominator = 1
    scd.BufferUsage = {.RENDER_TARGET_OUTPUT}
    scd.OutputWindow = handle
    scd.Windowed = true
    scd.SampleDesc.Count = 8
    scd.SampleDesc.Quality = 0

    creation_flags : dx11.CREATE_DEVICE_FLAGS

    when ODIN_DEBUG {
        creation_flags |= {.DEBUG}
    }

    driver_type : dx11.DRIVER_TYPE
    feature_lvl : dx11.FEATURE_LEVEL

    result : Renderer

    for dtype in driver_types {
        result := dx11.CreateDeviceAndSwapChain(nil, dtype, nil, creation_flags,
            raw_data(feature_levels), 3, dx11.SDK_VERSION,
            &scd, &result.swap_chain,
            &result.device,
            &feature_lvl, &result.dcontext)
        if (win.SUCCEEDED(result)) {
            driver_type = dtype
            break
        } else {
            fmt.panicf("ne radi ")
        }
    }

    assert(result.device != nil, "d3ddevice is null")

    bbuffer_texture : ^dx11.ITexture2D

    result1 := result.swap_chain->GetBuffer(0, dx11.ITexture2D_UUID,
        (^rawptr)(&bbuffer_texture))

    assert(win.SUCCEEDED(result1), "getbuffer")

    result1 = result.device->CreateRenderTargetView(bbuffer_texture, nil, &result.rview)

    bbuffer_texture->Release()

    assert(win.SUCCEEDED(result1), "create render target view")
    result.dcontext->OMSetRenderTargets(1, &result.rview, nil)

    dimensions : win.RECT
    win.GetClientRect(handle, &dimensions)
    client_width : u32 = u32(dimensions.right - dimensions.left)
    client_height : u32 = u32(dimensions.bottom - dimensions.top)

    viewport : dx11.VIEWPORT
    viewport.Width = f32(client_width)
    viewport.Height = f32(client_height)
    viewport.MaxDepth = 1.0

    result.dcontext->RSSetViewports(1, &viewport)


    vbatch, _ := virtual.arena_alloc(arena, size_of(VertexBatch), 1)
    //vertex_batch1, _ := virtual.arena_alloc(&arena, MAX_VERTICES * size_of(Vertex), 1)
    result.vbatch = cast(^VertexBatch)&vbatch[0]

    vdesc : dx11.BUFFER_DESC

    vdesc.Usage = .DYNAMIC
    vdesc.BindFlags = {.VERTEX_BUFFER}
    vdesc.ByteWidth = size_of(Vertex) * MAX_VERTICES
    vdesc.CPUAccessFlags = {.WRITE}

    result.device->CreateBuffer(&vdesc, nil, &result.vbuffer)

    vs_blob : ^dx11.IBlob

    ebuffer : ^dx11.IBlob
    //d3d.CompileFromFile()
    d3d.Compile(raw_data(shaders_hlsl), len(shaders_hlsl),
        "shaders.hlsl", nil, nil, "vs_main",
        "vs_5_0", 0, 0, &vs_blob, &ebuffer)

    if(ebuffer != nil) {
        log.error(ebuffer->GetBufferPointer())
    }

    assert(vs_blob != nil)
    result.device->CreateVertexShader(vs_blob->GetBufferPointer(),
        vs_blob->GetBufferSize(),
        nil, &result.vs)



    vlayout := [?]dx11.INPUT_ELEMENT_DESC {
        {"POS", 0, .R32G32B32_FLOAT, 0, 0,                           .VERTEX_DATA, 0},
        {"TEX", 0, .R32G32_FLOAT,    0, dx11.APPEND_ALIGNED_ELEMENT, .VERTEX_DATA, 0},
        {"COL", 0, .R32G32B32_FLOAT, 0, dx11.APPEND_ALIGNED_ELEMENT, .VERTEX_DATA, 0}
    }

    result.device->CreateInputLayout(&vlayout[0], len(vlayout), vs_blob->GetBufferPointer(),
        vs_blob->GetBufferSize(), &result.input_layout)

    ps_blob : ^dx11.IBlob

    d3d.Compile(raw_data(shaders_hlsl), len(shaders_hlsl), "shaders.hlsl",
        nil, nil, "ps_main", "ps_5_0", 0, 0, &ps_blob, nil)
    assert(ps_blob != nil)

    result.device->CreatePixelShader(ps_blob->GetBufferPointer(), ps_blob->GetBufferSize(),
        nil, &result.ps)


    rdesc := dx11.RASTERIZER_DESC {
        FillMode = .SOLID,
        CullMode = .NONE,
        FrontCounterClockwise = false,
        DepthClipEnable = true,
        MultisampleEnable = true,
        AntialiasedLineEnable = true,
        ScissorEnable = false
    }

    result.device->CreateRasterizerState(&rdesc, &result.rstate)

    twidth, theight, nr_channels : i32
    image_data := stbi.load("atlas.png", &twidth, &theight, &nr_channels, 4)
    assert(image_data != nil)

    texture_desc := dx11.TEXTURE2D_DESC{
        Width      = u32(twidth),
        Height     = u32(theight),
        MipLevels  = 1,
        ArraySize  = 1,
        Format     = .R8G8B8A8_UNORM,
        SampleDesc = {Count = 1},
        Usage      = .IMMUTABLE,
        BindFlags  = {.SHADER_RESOURCE},
    }

    texture_data := dx11.SUBRESOURCE_DATA{
        pSysMem     = &image_data[0],
        SysMemPitch = u32(twidth * 4),
    }

    texture : ^dx11.ITexture2D
    //cmap : ^dx11.IShaderResourceView
    result.device->CreateTexture2D(&texture_desc, &texture_data, &texture)

    result.device->CreateShaderResourceView(texture, nil, &result.cmap)

    stbi.image_free(image_data)


    cmap_desc := dx11.SAMPLER_DESC {
        AddressU = .CLAMP,
        AddressV = .CLAMP,
        AddressW = .CLAMP,
        ComparisonFunc = .NEVER,
        Filter = .MIN_MAG_MIP_POINT,
    }

    result.device->CreateSamplerState(&cmap_desc, &result.cmap_sampler)


    blend : ^dx11.IBlendState
    blend_desc : dx11.BLEND_DESC
    blend_desc.RenderTarget[0].BlendOp = .ADD
    blend_desc.RenderTarget[0].BlendEnable = win.TRUE
    blend_desc.RenderTarget[0].SrcBlend = .SRC_ALPHA
    blend_desc.RenderTarget[0].DestBlend = .INV_SRC_ALPHA
    blend_desc.RenderTarget[0].BlendOpAlpha = .ADD
    blend_desc.RenderTarget[0].SrcBlendAlpha = .ZERO
    blend_desc.RenderTarget[0].DestBlendAlpha = .ZERO
    blend_desc.RenderTarget[0].RenderTargetWriteMask = 0x0F

    result.device->CreateBlendState(&blend_desc, &blend)
    result.dcontext->OMSetBlendState(blend, &[4]f32{0, 0, 0, 0}, 0xFFFFFFFF)

    return result
}

get_texcoord :: proc(r : vec4) -> vec4 {
    tex : vec4
    tex.x = r.x  / ATLAS_WIDTH
    tex.y = (r.x  + r.z) / ATLAS_WIDTH
    tex.z = r.y / ATLAS_HEIGHT
    tex.w = (r.y  + r.w) / ATLAS_HEIGHT
    return tex
}

__draw :: proc(vb: ^VertexBatch, quad, tex : vec4, color : Color) {

    w1 := quad.x + quad.z
    h1 := quad.y + quad.w               //
    vappend(vb, {quad.x, quad.y, 0}, {tex.x, tex.z},color)
    vappend(vb, {w1,     quad.y, 0}, {tex.y, tex.z},color)
    vappend(vb, {w1,     h1,     0}, {tex.y, tex.w},color)
    vappend(vb, {w1,     h1,     0}, {tex.y, tex.w},color)
    vappend(vb, {quad.x, h1,     0}, {tex.x, tex.w},color)
    vappend(vb, {quad.x, quad.y, 0}, {tex.x, tex.z},color)
}

fill_quad :: proc(vb : ^VertexBatch, quad: vec4, color : Color) {

    tex := get_texcoord(atlas_glyphs[len(atlas_glyphs) - 8].rect)
    tex.x += 0.01
    tex.z += 0.02
    tex.yw -= 0.01
    __draw(vb, quad, tex, color)
}

fill_triangle :: proc(vb : ^VertexBatch, quad : vec4, color: Color) {
    w1 := quad.x + quad.z
    h1 := quad.y + quad.w

    tex := get_texcoord(atlas_glyphs[len(atlas_glyphs) - 8].rect)
    tex.x += 0.01
    tex.z += 0.02
    tex.yw -= 0.01
                   //
    vappend(vb, {quad.x + (quad.z/2), quad.y, 0}, {tex.x, tex.z},color)
    vappend(vb, {w1,                  h1,     0}, {tex.y, tex.z},color)
    vappend(vb, {quad.x,              h1,     0}, {tex.y, tex.w},color)
}


draw_line :: proc(vb: ^VertexBatch, start : vec2, end : vec2, color: Color, thickness : f32){
    tex := get_texcoord(atlas_glyphs[len(atlas_glyphs) - 8].rect)
    tex.x += 0.01
    tex.z += 0.02
    tex.yw -= 0.01

    vappend(vb, {start.x,    start.y, 0}, {tex.x, tex.z},color)
    vappend(vb, {start.x + thickness, start.y , 0}, {tex.y, tex.z},color)
    vappend(vb, {end.x,      end.y,     0}, {tex.y, tex.w},color)
    vappend(vb, {end.x,      end.y,     0}, {tex.y, tex.w},color)
    vappend(vb, {end.x - thickness,  end.y,     0}, {tex.x, tex.w},color)
    vappend(vb, {start.x,    start.y, 0}, {tex.x, tex.z},color)
}

draw_quad :: proc(vb : ^VertexBatch, quad: vec4, color : Color, thickness : f32) {

    tex := get_texcoord(atlas_glyphs[len(atlas_glyphs) - 8].rect)
    tex.x += 0.01
    tex.z += 0.02
    tex.yw -= 0.01

    quad1 := quad

    quad1.w = thickness

    __draw(vb, quad1, tex, color)

    quad1.y = (quad.y + quad.w) - thickness
    __draw(vb, quad1, tex, color)

    quad1 = quad
    quad1.z = thickness
    __draw(vb, quad1, tex, color)
    quad1.x = (quad.x + quad.z) - thickness
    __draw(vb, quad1, tex, color)
}

draw_texture :: proc (vb: ^VertexBatch, name : TextureName, r : vec4) {
    sprite1 := atlas_textures[name]

    tex := get_texcoord(sprite1.rect)
    __draw(vb, r, tex, WHITE)
}

draw_text_colored :: proc (vb : ^VertexBatch, pos : vec2, text : string, color: Color) {
    startx := pos.x
    starty := pos.y

    for slovo in text {
        fnt := atlas_glyphs[0]
        if slovo == ' ' {
            startx += 17
            continue
        }
        for glyph in atlas_glyphs {
            if glyph.value == slovo {
                fnt = glyph
                break
            }
        }

        if(slovo == '\n') {
            starty += fnt.rect.y / 2
            startx = pos.x
            continue
        }

        tex := get_texcoord(fnt.rect)
        starty1 := starty + f32(fnt.offset_y)
        startx1 := startx + f32(fnt.offset_x)

        endx := fnt.rect.z
        endy := fnt.rect.w

        __draw(vb, {startx1, starty1, endx, endy}, tex, color)

        startx += f32(fnt.advance_x)

    }
}

draw_text :: proc(vb : ^VertexBatch, pos : vec2, text : string) {
    draw_text_colored(vb, pos, text, WHITE)
}

check_collision :: proc(rec1, rec2 : vec4) -> b32 {
    return ((rec1.x < (rec2.x + rec2.z) && (rec1.x + rec1.z) > rec2.x) &&
            (rec1.y < (rec2.y + rec2.w) && (rec1.y + rec1.w) > rec2.y));
}

main :: proc() {
    context.logger = log.create_console_logger()

    default_allocator := context.allocator
    track: mem.Tracking_Allocator
    mem.tracking_allocator_init(&track, default_allocator)
    defer mem.tracking_allocator_destroy(&track)
    context.allocator = mem.tracking_allocator(&track)

    arena : virtual.Arena
    arr_err := virtual.arena_init_static(&arena)
    if arr_err != .None {
        panic("Arena not allocated")
    }
    defer virtual.arena_destroy(&arena)


    handle := create_window(1280, 720)

    running := true

    renderer := d3d_init(handle, &arena)

    dimensions : win.RECT
    win.GetClientRect(handle, &dimensions)
    client_width : f32 = f32(dimensions.right - dimensions.left)
    client_height : f32 = f32(dimensions.bottom - dimensions.top)

    proj := linalg.matrix_ortho3d_f32(0, client_width,
        client_height, 0, -1, 1)

    constant_buffer_desc := dx11.BUFFER_DESC{
        ByteWidth      = size_of(Constants),
        Usage          = .DYNAMIC,
        BindFlags      = {.CONSTANT_BUFFER},
        CPUAccessFlags = {.WRITE},
    }
    constant_buffer: ^dx11.IBuffer
    renderer.device->CreateBuffer(&constant_buffer_desc, nil, &constant_buffer)

    current : [20]TextureName
    index := 0

    anim_tick := time.tick_now()
    tick := anim_tick

    posp := vec2{64, 510}
    dx := vec2{0, 0}

    old_input : [Buttons]ButtonState
    new_input : [Buttons]ButtonState
    flip := false
    is_grounded := false

    obs : vec4 = {300, 600, 100, 20}

    for running {
        msg : win.MSG
        dt := f32(time.duration_seconds(time.tick_lap_time(&tick)))

        new_input = {}

        for inputs, ind in old_input {
            new_input[ind].ended_down = inputs.ended_down
        }

        for win.PeekMessageW(&msg, nil, 0, 0, win.PM_REMOVE) {
            vk_code := msg.wParam
            switch msg.message {
                case win.WM_QUIT : running = false;
                case win.WM_KEYUP :  fallthrough
                case win.WM_SYSKEYUP : fallthrough
                case win.WM_SYSKEYDOWN : fallthrough
                case win.WM_KEYDOWN : {

                    was_down := ((msg.lParam & (1 << 30)) != 0)
                    is_down  := ((msg.lParam & (1 << 31)) == 0)

                    if(is_down != was_down) {
                        switch vk_code {
                            case 'A' : fallthrough
                            case win.VK_LEFT : process_keyboard_message(&new_input[.Move_Left], is_down)
                            case 'D' : fallthrough
                            case win.VK_RIGHT : process_keyboard_message(&new_input[.Move_Right], is_down)
                            case 'W' : fallthrough
                            case win.VK_UP : process_keyboard_message(&new_input[.Move_Up], is_down)
                            case 'S' : fallthrough
                            case win.VK_DOWN : process_keyboard_message(&new_input[.Move_Down], is_down)
                            case win.VK_SPACE : process_keyboard_message(&new_input[.Move_Up], is_down)

                        }
                    }
                }
                case : {
                    win.TranslateMessage(&msg)
                    win.DispatchMessageW(&msg)
                }
            }
        }

        for xindex in 0..<win.XUSER_MAX_COUNT {
            controller_state : win.XINPUT_STATE
            if win.XInputGetState(nil, &controller_state) == .SUCCESS {
                //pad := &controller_state.Gamepad
            }
        }

        if !is_grounded { dx.y += 2000 * dt }
        idle := atlas_animations[.The_Harvester_Idle]

        if new_input[.Move_Left].ended_down == true {
            idle = atlas_animations[.The_Harvester_Walk]
            dx.x -= 205
            flip = true
        }
        if new_input[.Move_Right].ended_down {
            idle = atlas_animations[.The_Harvester_Walk]
            dx.x += 205
            flip = false
        }

        if(was_pressed(&new_input[.Move_Up]) && is_grounded) {
            dx.y = -800
            is_grounded = false
        }

        tmp_index := 0
        for id in idle.first_frame..=idle.last_frame {
            current[tmp_index] = id
            tmp_index += 1
        }

        if f32(time.duration_seconds(time.tick_since(anim_tick))) >
            atlas_textures[current[index]].duration {
            anim_tick = time.tick_now()
            index += 1
        }
        if index > int(idle.last_frame - idle.first_frame)   {index = 0}

        //posp += dx * f32(dt)
        texture_size := atlas_textures[current[index]].document_size

        if(posp.x < 0) {posp.x = 0}
        else if (posp.x + texture_size.x > client_width) {
            posp.x = client_width - texture_size.x
        }

        posp += dx * dt

        dx.x = 0
        //dx.y = 0

        if(posp.y + texture_size.y > client_height - 64) {
            posp.y = client_height - texture_size.y - 64
            is_grounded = true
        } else if (posp.y < 0) {posp.y = 0}

        chc := vec4{posp.x, posp.y, texture_size.x, texture_size.y}

        chc.z = chc.z - (chc.z / 2)
        if(!flip) { chc.x = chc.x + atlas_textures[current[index]].offset_right}

        texture_position : vec4 = {posp.x, posp.y, texture_size.x, texture_size.y}

        if(flip == true) {
            texture_position.x = posp.x + texture_size.x
            texture_position.z = -texture_size.x
        }

        for X in 0..<WORLD_WIDTH {
            for Y in 0..<WORLD_HEIGHT {
                tile := world[Y * WORLD_WIDTH + X]
                white :f32= 0
                if(tile == 1) {white = 1}
                else {white = 0.5}
                cmat := Color{white, white, white}
                xs := client_width / WORLD_WIDTH
                ys := client_height / WORLD_HEIGHT

                fill_quad(renderer.vbatch, {f32(X) * xs, f32(Y) * ys, xs, ys}, cmat)
            }
        }

        draw_texture(renderer.vbatch, current[index], texture_position)
        draw_quad(renderer.vbatch, chc, RED, 1)

        trans := linalg.MATRIX4F32_IDENTITY
        trans *= proj

        mpds : dx11.MAPPED_SUBRESOURCE


        renderer.dcontext->Map(constant_buffer, 0, .WRITE_DISCARD, {}, &mpds)
        {
            constants := (^Constants)(mpds.pData)
            constants.transform = trans
        }
        renderer.dcontext->Unmap(constant_buffer, 0)

        renderer.dcontext->Map(renderer.vbuffer, 0, .WRITE_DISCARD, {}, &mpds)
        {
            mem.copy(mpds.pData, raw_data(&renderer.vbatch.vertex),
                size_of(Vertex) * int(renderer.vbatch.size))
        }

        renderer.dcontext->Unmap(renderer.vbuffer, 0)

        renderer.dcontext->ClearRenderTargetView(renderer.rview, &[4]f32{0.3, 0.7, 0.2, 1})
        stride : u32 = size_of(Vertex)
        offset : u32 = 0

        renderer.dcontext->RSSetState(renderer.rstate)
        renderer.dcontext->IASetInputLayout(renderer.input_layout)
        renderer.dcontext->IASetVertexBuffers(0, 1, &renderer.vbuffer, &stride, &offset)
        renderer.dcontext->IASetPrimitiveTopology(.TRIANGLELIST)

        renderer.dcontext->VSSetShader(renderer.vs, nil, 0)
        renderer.dcontext->VSSetConstantBuffers(0, 1, &constant_buffer)
        renderer.dcontext->PSSetShader(renderer.ps, nil, 0)

        renderer.dcontext->PSSetShaderResources(0, 1, &renderer.cmap)
        renderer.dcontext->PSSetSamplers(0, 1, &renderer.cmap_sampler)


        renderer.dcontext->Draw(renderer.vbatch.size, 0)

        renderer.swap_chain->Present(1, nil)

        renderer.vbatch.size = 0;

        tmp_btnstate := old_input
        old_input = new_input
        new_input = tmp_btnstate

        free_all(context.temp_allocator)
    }
    d3d_clean(&renderer)
    free_all(context.temp_allocator)
    for _, value in track.allocation_map {
        fmt.printf("%v leaked %v bytes", value.location, value.size)
    }
}

shaders_hlsl := `
cbuffer constants : register(b0) {
    float4x4 transform;
}

Texture2D colormap : register(t0);
SamplerState color_sampler : register(s0);

struct vout {
    float4 position : SV_POSITION;
    float2 tex0 : TEX;
    float4 color : COL;
};

struct vin {
    float4 position: POS;
    float2 tex0 : TEX;
    float3 color : COL;
};

vout vs_main(vin input) {
    vout output;
    output.position = mul(transform, input.position);
    output.color = float4(input.color, 1.0f);
    output.tex0 = input.tex0;
    return output;
}
float4 ps_main(vout input) : SV_TARGET {
    return colormap.Sample(color_sampler, input.tex0) * input.color;
}`


// generated using https://github.com/karl-zylinski/atlas-builder.git

TEXTURE_ATLAS_FILENAME :: "atlas.png"
ATLAS_FONT_SIZE :: 32
ATLAS_WIDTH :: 511
ATLAS_HEIGHT :: 174
LETTERS_IN_FONT :: "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890?!&.,_:[]-+"

// A generated square in the atlas you can use with rl.SetShapesTexture to make
// raylib shapes such as rl.DrawRectangleRec() use the atlas.
SHAPES_TEXTURE_RECT :: vec4 {40, 164, 10, 10}

TextureName :: enum {
	None,
	The_Harvester0,
	The_Harvester1,
	The_Harvester2,
	The_Harvester3,
	The_Harvester4,
	The_Harvester5,
	The_Harvester6,
	The_Harvester7,
	The_Harvester8,
	The_Harvester9,
	The_Harvester10,
	The_Harvester11,
	The_Harvester12,
	The_Harvester13,
	The_Harvester14,
	The_Harvester15,
	The_Harvester16,
	The_Harvester17,
	The_Harvester18,
	The_Harvester19,
	The_Harvester20,
	The_Harvester21,
	The_Harvester22,
	The_Harvester23,
	The_Harvester24,
	The_Harvester25,
	The_Harvester26,
	The_Harvester27,
	The_Harvester28,
	The_Harvester29,
	The_Harvester30,
	The_Harvester31,
	The_Harvester32,
	The_Harvester33,
	The_Harvester34,
	The_Harvester35,
	The_Harvester36,
	The_Harvester37,
	The_Harvester38,
	The_Harvester39,
	The_Harvester40,
	The_Harvester41,
	The_Harvester42,
	The_Harvester43,
	The_Harvester44,
	The_Harvester45,
	The_Harvester46,
	The_Harvester47,
	The_Harvester48,
	The_Harvester49,
	The_Harvester50,
	The_Harvester51,
	The_Harvester52,
	The_Harvester53,
	The_Harvester54,
	The_Harvester55,
	The_Harvester56,
	The_Harvester57,
	The_Harvester58,
	The_Harvester59,
	The_Harvester60,
	The_Harvester61,
	The_Harvester62,
	The_Harvester63,
	The_Harvester64,
	The_Harvester65,
	The_Harvester66,
	The_Harvester67,
	The_Harvester68,
	The_Harvester69,
	The_Harvester70,
	The_Harvester71,
}

AtlasTexture :: struct {
	rect: vec4,
	// These offsets tell you how much space there is between the rect and the edge of the original document.
	// The atlas is tightly packed, so empty pixels are removed. This can be especially apparent in animations where
	// frames can have different offsets due to different amount of empty pixels around the frames.
	// In many cases you need to add {offset_left, offset_top} to your position. But if you are
	// flipping a texture, then you might need offset_bottom or offset_right.
	offset_top: f32,
	offset_right: f32,
	offset_bottom: f32,
	offset_left: f32,
	document_size: vec2,
	duration: f32,
}

atlas_textures: [TextureName]AtlasTexture = {
	.None = {},
	.The_Harvester0 = { rect = {89, 124, 27, 20}, offset_top = 45, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.100},
	.The_Harvester1 = { rect = {61, 124, 27, 20}, offset_top = 45, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.100},
	.The_Harvester2 = { rect = {478, 102, 27, 20}, offset_top = 45, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.100},
	.The_Harvester3 = { rect = {229, 124, 27, 20}, offset_top = 45, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.100},
	.The_Harvester4 = { rect = {0, 86, 27, 21}, offset_top = 44, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.100},
	.The_Harvester5 = { rect = {309, 102, 27, 21}, offset_top = 44, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.100},
	.The_Harvester6 = { rect = {478, 123, 27, 20}, offset_top = 45, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.100},
	.The_Harvester7 = { rect = {453, 144, 27, 20}, offset_top = 45, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.100},
	.The_Harvester8 = { rect = {0, 129, 27, 20}, offset_top = 45, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.100},
	.The_Harvester9 = { rect = {482, 79, 27, 22}, offset_top = 43, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.100},
	.The_Harvester10 = { rect = {341, 124, 27, 20}, offset_top = 45, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.100},
	.The_Harvester11 = { rect = {313, 124, 27, 20}, offset_top = 45, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.100},
	.The_Harvester12 = { rect = {366, 79, 27, 23}, offset_top = 42, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.100},
	.The_Harvester13 = { rect = {173, 124, 27, 20}, offset_top = 45, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.100},
	.The_Harvester14 = { rect = {145, 124, 27, 20}, offset_top = 45, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.100},
	.The_Harvester15 = { rect = {393, 103, 27, 20}, offset_top = 45, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.100},
	.The_Harvester16 = { rect = {0, 62, 27, 23}, offset_top = 42, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.100},
	.The_Harvester17 = { rect = {159, 79, 27, 23}, offset_top = 42, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.100},
	.The_Harvester18 = { rect = {394, 79, 27, 23}, offset_top = 42, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.100},
	.The_Harvester19 = { rect = {73, 102, 28, 21}, offset_top = 44, offset_right = 46, offset_bottom = 21, offset_left = 31, document_size = {105, 86}, duration = 0.100},
	.The_Harvester20 = { rect = {159, 55, 31, 23}, offset_top = 42, offset_right = 46, offset_bottom = 21, offset_left = 28, document_size = {105, 86}, duration = 0.100},
	.The_Harvester21 = { rect = {200, 0, 34, 26}, offset_top = 39, offset_right = 44, offset_bottom = 21, offset_left = 27, document_size = {105, 86}, duration = 0.100},
	.The_Harvester22 = { rect = {28, 129, 27, 20}, offset_top = 45, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.05},
	.The_Harvester23 = { rect = {112, 145, 27, 19}, offset_top = 46, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.05},
	.The_Harvester24 = { rect = {84, 145, 27, 19}, offset_top = 46, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.05},
	.The_Harvester25 = { rect = {117, 124, 27, 20}, offset_top = 45, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.05},
	.The_Harvester26 = { rect = {422, 102, 27, 21}, offset_top = 44, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.05},
	.The_Harvester27 = { rect = {337, 102, 27, 21}, offset_top = 44, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.05},
	.The_Harvester28 = { rect = {201, 124, 27, 20}, offset_top = 45, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.05},
	.The_Harvester29 = { rect = {56, 145, 27, 19}, offset_top = 46, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.05},
	.The_Harvester30 = { rect = {140, 145, 27, 19}, offset_top = 46, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.05},
	.The_Harvester31 = { rect = {285, 124, 27, 20}, offset_top = 45, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.05},
	.The_Harvester32 = { rect = {281, 102, 27, 21}, offset_top = 44, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.05},
	.The_Harvester33 = { rect = {131, 102, 27, 21}, offset_top = 44, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.05},
	.The_Harvester34 = { rect = {369, 124, 27, 20}, offset_top = 45, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.05},
	.The_Harvester35 = { rect = {397, 124, 27, 20}, offset_top = 45, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.100},
	.The_Harvester36 = { rect = {168, 145, 27, 19}, offset_top = 46, offset_right = 48, offset_bottom = 21, offset_left = 30, document_size = {105, 86}, duration = 0.100},
	.The_Harvester37 = { rect = {102, 102, 28, 21}, offset_top = 44, offset_right = 49, offset_bottom = 21, offset_left = 28, document_size = {105, 86}, duration = 0.100},
	.The_Harvester38 = { rect = {251, 81, 29, 21}, offset_top = 44, offset_right = 49, offset_bottom = 21, offset_left = 27, document_size = {105, 86}, duration = 0.100},
	.The_Harvester39 = { rect = {187, 81, 63, 21}, offset_top = 44, offset_right = 13, offset_bottom = 21, offset_left = 29, document_size = {105, 86}, duration = 0.100},
	.The_Harvester40 = { rect = {286, 80, 63, 21}, offset_top = 44, offset_right = 13, offset_bottom = 21, offset_left = 29, document_size = {105, 86}, duration = 0.100},
	.The_Harvester41 = { rect = {73, 80, 66, 21}, offset_top = 44, offset_right = 8, offset_bottom = 21, offset_left = 31, document_size = {105, 86}, duration = 0.100},
	.The_Harvester42 = { rect = {450, 102, 27, 21}, offset_top = 44, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.100},
	.The_Harvester43 = { rect = {221, 103, 57, 20}, offset_top = 45, offset_right = 18, offset_bottom = 21, offset_left = 30, document_size = {105, 86}, duration = 0.100},
	.The_Harvester44 = { rect = {0, 108, 60, 20}, offset_top = 45, offset_right = 17, offset_bottom = 21, offset_left = 28, document_size = {105, 86}, duration = 0.100},
	.The_Harvester45 = { rect = {159, 103, 61, 20}, offset_top = 45, offset_right = 17, offset_bottom = 21, offset_left = 27, document_size = {105, 86}, duration = 0.100},
	.The_Harvester46 = { rect = {365, 103, 27, 20}, offset_top = 45, offset_right = 45, offset_bottom = 21, offset_left = 33, document_size = {105, 86}, duration = 0.100},
	.The_Harvester47 = { rect = {257, 124, 27, 20}, offset_top = 45, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.100},
	.The_Harvester48 = { rect = {425, 124, 27, 20}, offset_top = 45, offset_right = 46, offset_bottom = 21, offset_left = 32, document_size = {105, 86}, duration = 0.100},
	.The_Harvester49 = { rect = {196, 145, 27, 19}, offset_top = 46, offset_right = 48, offset_bottom = 21, offset_left = 30, document_size = {105, 86}, duration = 0.100},
	.The_Harvester50 = { rect = {123, 55, 17, 24}, offset_top = 41, offset_right = 49, offset_bottom = 21, offset_left = 39, document_size = {105, 86}, duration = 0.100},
	.The_Harvester51 = { rect = {105, 55, 17, 24}, offset_top = 41, offset_right = 49, offset_bottom = 21, offset_left = 39, document_size = {105, 86}, duration = 0.100},
	.The_Harvester52 = { rect = {69, 55, 17, 24}, offset_top = 41, offset_right = 49, offset_bottom = 21, offset_left = 39, document_size = {105, 86}, duration = 0.100},
	.The_Harvester53 = { rect = {33, 55, 17, 24}, offset_top = 41, offset_right = 49, offset_bottom = 21, offset_left = 39, document_size = {105, 86}, duration = 0.100},
	.The_Harvester54 = { rect = {51, 55, 17, 24}, offset_top = 41, offset_right = 49, offset_bottom = 21, offset_left = 39, document_size = {105, 86}, duration = 0.100},
	.The_Harvester55 = { rect = {481, 54, 17, 24}, offset_top = 41, offset_right = 49, offset_bottom = 21, offset_left = 39, document_size = {105, 86}, duration = 0.100},
	.The_Harvester56 = { rect = {463, 54, 17, 24}, offset_top = 41, offset_right = 49, offset_bottom = 21, offset_left = 39, document_size = {105, 86}, duration = 0.100},
	.The_Harvester57 = { rect = {87, 55, 17, 24}, offset_top = 41, offset_right = 49, offset_bottom = 21, offset_left = 39, document_size = {105, 86}, duration = 0.100},
	.The_Harvester58 = { rect = {141, 55, 17, 24}, offset_top = 41, offset_right = 49, offset_bottom = 21, offset_left = 39, document_size = {105, 86}, duration = 0.100},
	.The_Harvester59 = { rect = {440, 54, 22, 24}, offset_top = 41, offset_right = 44, offset_bottom = 21, offset_left = 39, document_size = {105, 86}, duration = 0.100},
	.The_Harvester60 = { rect = {28, 80, 44, 22}, offset_top = 43, offset_right = 32, offset_bottom = 21, offset_left = 29, document_size = {105, 86}, duration = 0.100},
	.The_Harvester61 = { rect = {422, 79, 59, 22}, offset_top = 43, offset_right = 25, offset_bottom = 21, offset_left = 21, document_size = {105, 86}, duration = 0.100},
	.The_Harvester62 = { rect = {366, 54, 73, 24}, offset_top = 41, offset_right = 19, offset_bottom = 21, offset_left = 13, document_size = {105, 86}, duration = 0.100},
	.The_Harvester63 = { rect = {286, 54, 79, 25}, offset_top = 40, offset_right = 18, offset_bottom = 21, offset_left = 8, document_size = {105, 86}, duration = 0.100},
	.The_Harvester64 = { rect = {34, 0, 85, 27}, offset_top = 38, offset_right = 15, offset_bottom = 21, offset_left = 5, document_size = {105, 86}, duration = 0.100},
	.The_Harvester65 = { rect = {84, 165, 6, 6}, offset_top = 55, offset_right = 54, offset_bottom = 25, offset_left = 45, document_size = {105, 86}, duration = 0.100},
	.The_Harvester66 = { rect = {91, 165, 8, 5}, offset_top = 60, offset_right = 53, offset_bottom = 21, offset_left = 44, document_size = {105, 86}, duration = 0.100},
	.The_Harvester67 = { rect = {100, 165, 13, 4}, offset_top = 60, offset_right = 50, offset_bottom = 22, offset_left = 42, document_size = {105, 86}, duration = 0.100},
	.The_Harvester68 = { rect = {114, 165, 21, 2}, offset_top = 63, offset_right = 45, offset_bottom = 21, offset_left = 39, document_size = {105, 86}, duration = 0.100},
	.The_Harvester69 = { rect = {136, 165, 23, 1}, offset_top = 64, offset_right = 44, offset_bottom = 21, offset_left = 38, document_size = {105, 86}, duration = 0.100},
	.The_Harvester70 = { rect = {160, 165, 17, 1}, offset_top = 64, offset_right = 47, offset_bottom = 21, offset_left = 41, document_size = {105, 86}, duration = 0.100},
	.The_Harvester71 = { rect = {451, 145, 1, 1}, offset_top = 64, offset_right = 48, offset_bottom = 21, offset_left = 56, document_size = {105, 86}, duration = 0.100},
}

AnimationName :: enum {
	None,
	The_Harvester_Idle,
	The_Harvester_Startup,
	The_Harvester_Walk,
	The_Harvester_Attack,
	The_Harvester_Damage,
	The_Harvester_Death,
}

Tag_Loop_Dir :: enum {
	Forward,
	Reverse,
	Ping_Pong,
	Ping_Pong_Reverse,
}

// Any aseprite file with frames will create new animations. Also, any tags
// within the aseprite file will make that that into a separate animation.
AtlasAnimation :: struct {
	first_frame: TextureName,
	last_frame: TextureName,
	document_size: [2]f32,
	loop_direction: Tag_Loop_Dir,
	repeat: u16,
}

atlas_animations := [AnimationName]AtlasAnimation {
	.None = {},
	.The_Harvester_Idle = { first_frame = .The_Harvester1, last_frame = .The_Harvester14, loop_direction = .Forward, repeat = 0, document_size = {0, 0} },
	.The_Harvester_Startup = { first_frame = .The_Harvester15, last_frame = .The_Harvester21, loop_direction = .Forward, repeat = 0, document_size = {0, 0} },
	.The_Harvester_Walk = { first_frame = .The_Harvester22, last_frame = .The_Harvester34, loop_direction = .Forward, repeat = 0, document_size = {0, 0} },
	.The_Harvester_Attack = { first_frame = .The_Harvester35, last_frame = .The_Harvester47, loop_direction = .Forward, repeat = 0, document_size = {0, 0} },
	.The_Harvester_Damage = { first_frame = .The_Harvester48, last_frame = .The_Harvester52, loop_direction = .Forward, repeat = 0, document_size = {0, 0} },
	.The_Harvester_Death = { first_frame = .The_Harvester53, last_frame = .The_Harvester71, loop_direction = .Forward, repeat = 0, document_size = {0, 0} },
}

AtlasGlyph :: struct {
	rect: vec4,
	value: rune,
	offset_x: int,
	offset_y: int,
	advance_x: int,
}

atlas_glyphs: []AtlasGlyph = {
	{ rect = {215, 55, 18, 25}, value = 'A', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {115, 29, 18, 25}, value = 'B', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {461, 28, 18, 25}, value = 'C', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {401, 28, 18, 25}, value = 'D', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {321, 28, 18, 25}, value = 'E', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {281, 28, 18, 25}, value = 'F', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {487, 1, 18, 25}, value = 'G', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {201, 28, 18, 25}, value = 'H', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {274, 55, 11, 25}, value = 'I', offset_x = 0, offset_y = 0, advance_x = 17},
	{ rect = {248, 55, 11, 25}, value = 'J', offset_x = 0, offset_y = 0, advance_x = 17},
	{ rect = {447, 1, 18, 25}, value = 'K', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {287, 1, 18, 25}, value = 'L', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {307, 1, 18, 25}, value = 'M', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {327, 1, 18, 25}, value = 'N', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {347, 1, 18, 25}, value = 'O', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {367, 1, 18, 25}, value = 'P', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {387, 1, 18, 25}, value = 'Q', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {407, 1, 18, 25}, value = 'R', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {427, 1, 18, 25}, value = 'S', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {361, 28, 18, 25}, value = 'T', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {467, 1, 18, 25}, value = 'U', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {135, 29, 18, 25}, value = 'V', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {236, 1, 25, 25}, value = 'W', offset_x = 0, offset_y = 0, advance_x = 32},
	{ rect = {221, 28, 18, 25}, value = 'X', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {241, 28, 18, 25}, value = 'Y', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {261, 28, 18, 25}, value = 'Z', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {279, 146, 18, 18}, value = 'a', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {301, 28, 18, 25}, value = 'b', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {359, 146, 18, 18}, value = 'c', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {341, 28, 18, 25}, value = 'd', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {379, 146, 18, 18}, value = 'e', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {21, 36, 11, 25}, value = 'f', offset_x = 0, offset_y = 0, advance_x = 17},
	{ rect = {121, 1, 18, 26}, value = 'g', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {421, 28, 18, 25}, value = 'h', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {501, 28, 4, 25}, value = 'i', offset_x = 0, offset_y = 0, advance_x = 10},
	{ rect = {1, 1, 12, 33}, value = 'j', offset_x = -4, offset_y = 0, advance_x = 14},
	{ rect = {481, 28, 18, 25}, value = 'k', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {507, 1, 4, 25}, value = 'l', offset_x = 0, offset_y = 0, advance_x = 10},
	{ rect = {252, 146, 25, 18}, value = 'm', offset_x = 0, offset_y = 6, advance_x = 32},
	{ rect = {299, 146, 18, 18}, value = 'n', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {21, 151, 18, 18}, value = 'o', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {161, 1, 18, 26}, value = 'p', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {141, 1, 18, 26}, value = 'q', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {439, 146, 11, 18}, value = 'r', offset_x = 0, offset_y = 6, advance_x = 17},
	{ rect = {1, 151, 18, 18}, value = 's', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {235, 55, 11, 25}, value = 't', offset_x = 0, offset_y = 0, advance_x = 17},
	{ rect = {419, 146, 18, 18}, value = 'u', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {399, 146, 18, 18}, value = 'v', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {225, 146, 25, 18}, value = 'w', offset_x = 0, offset_y = 6, advance_x = 32},
	{ rect = {339, 146, 18, 18}, value = 'x', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {181, 1, 18, 26}, value = 'y', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {319, 146, 18, 18}, value = 'z', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {261, 55, 11, 25}, value = '1', offset_x = 0, offset_y = 0, advance_x = 17},
	{ rect = {195, 55, 18, 25}, value = '2', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {1, 36, 18, 25}, value = '3', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {175, 29, 18, 25}, value = '4', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {155, 29, 18, 25}, value = '5', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {95, 29, 18, 25}, value = '6', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {75, 29, 18, 25}, value = '7', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {55, 29, 18, 25}, value = '8', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {35, 29, 18, 25}, value = '9', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {441, 28, 18, 25}, value = '0', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {381, 28, 18, 25}, value = '?', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {507, 28, 4, 25}, value = '!', offset_x = 0, offset_y = 0, advance_x = 10},
	{ rect = {263, 1, 22, 25}, value = '&', offset_x = 0, offset_y = 0, advance_x = 28},
	{ rect = {502, 165, 8, 8}, value = '.', offset_x = 0, offset_y = 16, advance_x = 14},
	{ rect = {41, 151, 8, 12}, value = ',', offset_x = 0, offset_y = 16, advance_x = 14},
	{ rect = {52, 166, 18, 5}, value = '_', offset_x = 0, offset_y = 27, advance_x = 24},
	{ rect = {502, 145, 8, 18}, value = ':', offset_x = 0, offset_y = 6, advance_x = 14},
	{ rect = {15, 1, 8, 33}, value = '[', offset_x = 0, offset_y = 0, advance_x = 14},
	{ rect = {25, 1, 8, 33}, value = ']', offset_x = 0, offset_y = 0, advance_x = 14},
	{ rect = {72, 166, 11, 5}, value = '-', offset_x = 0, offset_y = 9, advance_x = 17},
	{ rect = {482, 145, 18, 19}, value = '+', offset_x = 0, offset_y = 2, advance_x = 24},
}
