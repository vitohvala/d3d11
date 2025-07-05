package win32_project0

import "core:fmt"
import win "core:sys/windows"
import "core:slice"
import "core:mem"
import "core:os"
import "core:strings"
import "core:strconv"
import "base:runtime"
import "core:log"
import "core:time"
import dx11 "vendor:directx/d3d11"
import dxgi "vendor:directx/dxgi"
import d3d  "vendor:directx/d3d_compiler"
import stbi "vendor:stb/image"
import "core:math"
import "core:math/linalg"


// TODO :
// indexed vertices

ATLAS := #load("resources/atlas.png")

DxStruct :: struct {
    device : dx11.IDevice,
    swap_chain : dxgi.ISwapChain,
    dev_context : dx11.IDeviceContext,
}

TextureName :: enum {
	None,
	Sprite_0001,
	Sprite_0002,
}

Atlas_Texture :: struct {
	rect: Rect,
	offset_top: f32,
	offset_right: f32,
	offset_bottom: f32,
	offset_left: f32,
	document_size: [2]f32,
	duration: f32,
}


TexCoord :: struct {
    x1, x2 : f32,
    y1, y2 : f32
}

Color :: [3]f32

RED :: Color{1, 0, 0}
WHITE :: Color{1, 1, 1}
BLACK :: Color{0, 0, 0}
BLUE :: Color{0, 0, 1}
GREEN :: Color{0, 1, 0}

Rect :: struct {
    x, y, w, h : f32,
}

Atlas_Glyph :: struct {
	rect: Rect,
	value: rune,
	offset_x: int,
	offset_y: int,
	advance_x: int,
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
        case win.WM_PAINT : {
            ps : win.PAINTSTRUCT
            hdc := win.BeginPaint(hwnd, &ps)
            win.EndPaint(hwnd, &ps)
        } return 0
        case win.WM_SIZE : {
        } return 0

    }

    return win.DefWindowProcW(hwnd, msg, wparam, lparam)
}

kilobytes :: proc(value: $T) -> T {
    return (value) * 1024
}

megabytes :: proc(value: $T) -> T {
    return kilobytes(value) * 1024
}

d3d_clean :: proc(dx : DxStruct) {

}

get_vert_char :: proc(ch : u8) -> []f32 {
    vert := make([]f32, 8 * 6)
    vert = {}
    return vert
}

get_texcoord :: proc(r : Rect) -> TexCoord {
    tex : TexCoord
    tex.x1 = r.x  / 510
    tex.x2 = (r.x  + r.w) / 510
    tex.y1 = r.y / 80
    tex.y2 = (r.y  + r.h) / 80
    return tex
}

draw_text :: proc(s : string, x, y : f32) {
    draw_text_color(s, x, y, WHITE)
}

draw_text_color :: proc(s : string, x, y : f32, c : Color) {
    startx := x
    starty := y
    for h in s {
        fnt := atlas_glyphs[0]
        for f in atlas_glyphs {
            if h == f.value {
                fnt = f
                break
            }
        }

        tex := get_texcoord(fnt.rect)
        starty1 := starty + f32(fnt.offset_y)
        startx1 := startx + f32(fnt.offset_x)
        fw := fnt.rect.w
        fh := fnt.rect.h

        append(&vertices, startx1 + fw, starty1,      0, tex.x2, tex.y1,  c.x, c.y, c.z)
        append(&vertices, startx1 + fw, starty1 + fh, 0, tex.x2, tex.y2,  c.x, c.y, c.z)
        append(&vertices, startx1,      starty1 + fh, 0, tex.x1, tex.y2,  c.x, c.y, c.z)
        append(&vertices, startx1,      starty1 + fh, 0, tex.x1, tex.y2,  c.x, c.y, c.z)
        append(&vertices, startx1,      starty1,      0, tex.x1, tex.y1,  c.x, c.y, c.z)
        append(&vertices, startx1 + fw, starty1,      0, tex.x2, tex.y1,  c.x, c.y, c.z)

        startx += f32(fnt.advance_x)
    }
}

draw_rectangle :: proc(rect : Rect, c : Color) {
    startx := rect.x
    starty := rect.y
    fnt := atlas_glyphs[len(atlas_glyphs) - 8]

    tex := get_texcoord(fnt.rect)
    starty1 := starty + f32(fnt.offset_y)
    startx1 := startx + f32(fnt.offset_x)
    fw := rect.w
    fh := rect.h

    tex.x1 += 0.01
    tex.x2 -= 0.01

    tex.y1 += 0.02
    tex.y2 -= 0.01

    append(&vertices, startx1 + fw, starty1,      0, tex.x2, tex.y1,  c.x, c.y, c.z)
    append(&vertices, startx1 + fw, starty1 + fh, 0, tex.x2, tex.y2,  c.x, c.y, c.z)
    append(&vertices, startx1,      starty1 + fh, 0, tex.x1, tex.y2,  c.x, c.y, c.z)
    append(&vertices, startx1,      starty1 + fh, 0, tex.x1, tex.y2,  c.x, c.y, c.z)
    append(&vertices, startx1,      starty1,      0, tex.x1, tex.y1,  c.x, c.y, c.z)
    append(&vertices, startx1 + fw, starty1,      0, tex.x2, tex.y1,  c.x, c.y, c.z)
}

draw_texture :: proc (name : TextureName, r : Rect) {
    sprite1 := atlas_textures[name]

    tex := get_texcoord(sprite1.rect)


    //   position      texc   color
    startx := r.x
    starty := r.y
    w := startx + r.w
    h := starty + r.h

    append(&vertices, w,      starty, 0, tex.x2, tex.y1,  1, 1, 1)
    append(&vertices, w,      h,      0, tex.x2, tex.y2,  1, 1, 1)
    append(&vertices, startx, h,      0, tex.x1, tex.y2,  1, 1, 1)
    append(&vertices, startx, h,      0, tex.x1, tex.y2,  1, 1, 1)
    append(&vertices, startx, starty, 0, tex.x1, tex.y1,  1, 1, 1)
    append(&vertices, w,      starty, 0, tex.x2, tex.y1,  1, 1, 1)

}

main :: proc() {
    context.logger = log.create_console_logger()

    default_allocator := context.allocator
    track: mem.Tracking_Allocator
	mem.tracking_allocator_init(&track, default_allocator)
	defer mem.tracking_allocator_destroy(&track)
	context.allocator = mem.tracking_allocator(&track)


    instance := win.HINSTANCE(win.GetModuleHandleW(nil))
    assert(instance != nil)

    wca : win.WNDCLASSW
    wca.hInstance = instance
    wca.lpszClassName = win.L("Project0")
    wca.style = win.CS_HREDRAW | win.CS_VREDRAW | win.CS_OWNDC
    wca.lpfnWndProc = win_proc

    cls := win.RegisterClassW(&wca)
    assert(cls != 0, "Class creation failed")

    handle := win.CreateWindowExW(0, wca.lpszClassName,
                                  win.L("Project-0"),
                                  win.WS_VISIBLE | win.WS_OVERLAPPEDWINDOW,
                                  win.CW_USEDEFAULT, win.CW_USEDEFAULT,
                                  1280, 720,
                                  nil, nil, instance, nil)

    assert(handle != nil)

    running := true

    dimensions : win.RECT
    win.GetClientRect(handle, &dimensions)
    width : u32 = u32(dimensions.right - dimensions.left)
    height : u32 = u32(dimensions.bottom - dimensions.top)

    driver_types := []dx11.DRIVER_TYPE{.HARDWARE, .WARP, .SOFTWARE}
    feature_levels := []dx11.FEATURE_LEVEL{ ._11_0, ._10_1, ._10_0}

    scd : dxgi.SWAP_CHAIN_DESC
    scd.BufferCount = 1
    scd.BufferDesc.Width = width
    scd.BufferDesc.Height = height
    scd.BufferDesc.Format = .R8G8B8A8_UNORM
    scd.BufferDesc.RefreshRate.Numerator = 60
    scd.BufferDesc.RefreshRate.Denominator = 1
    scd.BufferUsage = {.RENDER_TARGET_OUTPUT}
    scd.OutputWindow = handle
    scd.Windowed = true
    scd.SampleDesc.Count = 1
    //scd.SampleDesc.Quality = 0

    device : ^dx11.IDevice
    d3context : ^dx11.IDeviceContext
    swap_chain : ^dxgi.ISwapChain

    creation_flags : dx11.CREATE_DEVICE_FLAGS

    when ODIN_DEBUG {
        creation_flags |= {.DEBUG}
    }

    driver_type : dx11.DRIVER_TYPE
    feature_lvl : dx11.FEATURE_LEVEL

    for dtype in driver_types {
        result := dx11.CreateDeviceAndSwapChain(nil, dtype, nil, creation_flags,
                                                raw_data(feature_levels), 3, dx11.SDK_VERSION,
                                                &scd, &swap_chain, &device,
                                                &feature_lvl, &d3context)
        if (win.SUCCEEDED(result)) {
            driver_type = dtype
            break
        }
    }

    bbuffer_target : ^dx11.IRenderTargetView
    bbuffer_texture : ^dx11.ITexture2D

    result := swap_chain->GetBuffer(0, dx11.ITexture2D_UUID,
                                   (^rawptr)(&bbuffer_texture))

    assert(win.SUCCEEDED(result), "getbuffer")

    result = device->CreateRenderTargetView(bbuffer_texture, nil, &bbuffer_target)

    bbuffer_texture->Release()

    assert(win.SUCCEEDED(result), "create rendewr target view")
    d3context->OMSetRenderTargets(1, &bbuffer_target, nil)

    viewport : dx11.VIEWPORT
    viewport.Width = f32(width)
    viewport.Height = f32(height)
    viewport.MaxDepth = 1.0



    d3context->RSSetViewports(1, &viewport)

    sprite1 := Rect {450, 54, 16, 14}

    tex := get_texcoord(sprite1)


    //   position      texc   color

    append(&vertices, 200,  200, 0, tex.x2, tex.y1,  1, 1, 1)
    append(&vertices, 200,  216, 0, tex.x2, tex.y2,  1, 1, 1)
    append(&vertices, 184,  216, 0, tex.x1, tex.y2,  1, 1, 1)
    append(&vertices, 184,  216, 0, tex.x1, tex.y2,  1, 1, 1)
    append(&vertices, 184,  200, 0, tex.x1, tex.y1,  1, 1, 1)
    append(&vertices, 200,  200, 0, tex.x2, tex.y1,  1, 1, 1)



    vdesc : dx11.BUFFER_DESC

    vdesc.BindFlags = {.VERTEX_BUFFER}
    vdesc.Usage = .DYNAMIC
    vdesc.ByteWidth = u32(len(vertices) * size_of(f32))
    vdesc.CPUAccessFlags = {.WRITE}

    srdata : dx11.SUBRESOURCE_DATA

    srdata.pSysMem = raw_data(vertices)

    vbuffer : ^dx11.IBuffer
    device->CreateBuffer(&vdesc, &srdata, &vbuffer)

    vs : ^dx11.IVertexShader
    ps : ^dx11.IPixelShader
    input_layout : ^dx11.IInputLayout
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
    device->CreateVertexShader(vs_blob->GetBufferPointer(), vs_blob->GetBufferSize(),
                               nil, &vs)



    vlayout := [?]dx11.INPUT_ELEMENT_DESC {
        {"POS", 0, .R32G32B32_FLOAT, 0, 0,                           .VERTEX_DATA, 0},
        {"TEX", 0, .R32G32_FLOAT,    0, dx11.APPEND_ALIGNED_ELEMENT, .VERTEX_DATA, 0},
        {"COL", 0, .R32G32B32_FLOAT, 0, dx11.APPEND_ALIGNED_ELEMENT, .VERTEX_DATA, 0}
    }

    device->CreateInputLayout(&vlayout[0], len(vlayout), vs_blob->GetBufferPointer(),
                              vs_blob->GetBufferSize(), &input_layout)

    ps_blob : ^dx11.IBlob

    d3d.Compile(raw_data(shaders_hlsl), len(shaders_hlsl), "shaders.hlsl",
                nil, nil, "ps_main", "ps_5_0", 0, 0, &ps_blob, nil)
    assert(ps_blob != nil)

    device->CreatePixelShader(ps_blob->GetBufferPointer(), ps_blob->GetBufferSize(),
                              nil, &ps)

    cmap : ^dx11.IShaderResourceView

    twidth, theight, nr_channels: i32
    image_data := stbi.load_from_memory(&ATLAS[0], i32(len(ATLAS)), &twidth, &theight, &nr_channels, 4)
    assert(image_data != nil)

    //fmt.println(twidth)

    texture_desc := dx11.TEXTURE2D_DESC{
		Width      = u32(twidth),
		Height     = u32(theight),
		MipLevels  = 1,
		ArraySize  = 1,
		Format     = .R8G8B8A8_UNORM_SRGB,
		SampleDesc = {Count = 1},
		Usage      = .IMMUTABLE,
		BindFlags  = {.SHADER_RESOURCE},
	}

	texture_data := dx11.SUBRESOURCE_DATA{
		pSysMem     = &image_data[0],
		SysMemPitch = u32(twidth * 4),
	}

    texture : ^dx11.ITexture2D
    device->CreateTexture2D(&texture_desc, &texture_data, &texture)

    device->CreateShaderResourceView(texture, nil, &cmap)

    cmap_desc := dx11.SAMPLER_DESC {
        AddressU = .WRAP,
        AddressV = .WRAP,
        AddressW = .WRAP,
        ComparisonFunc = .NEVER,
        Filter = .MIN_MAG_MIP_LINEAR,
    }

    cmap_sampler : ^dx11.ISamplerState
    device->CreateSamplerState(&cmap_desc, &cmap_sampler)

    stbi.image_free(image_data)

    tick := time.tick_now()

    Constants :: struct #align(16) {
        transform : matrix[4, 4]f32,
        time : f32,
    }

    constant_buffer_desc := dx11.BUFFER_DESC{
		ByteWidth      = size_of(Constants),
		Usage          = .DYNAMIC,
		BindFlags      = {.CONSTANT_BUFFER},
		CPUAccessFlags = {.WRITE},
	}
	constant_buffer: ^dx11.IBuffer
	device->CreateBuffer(&constant_buffer_desc, nil, &constant_buffer)

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

    device->CreateBlendState(&blend_desc, &blend)
    d3context->OMSetBlendState(blend, &[4]f32{0, 0, 0, 0}, 0xFFFFFFFF)

    proj := linalg.matrix_ortho3d_f32(0, f32(width), f32(height), 0, -1, 1)


    for running {
        msg : win.MSG

        for win.PeekMessageW(&msg, nil, 0, 0, win.PM_REMOVE) {
            if msg.message == win.WM_QUIT {
                running = false
                break;
            }
            win.DispatchMessageW(&msg)
        }

        for xindex in 0..<win.XUSER_MAX_COUNT {
            controller_state : win.XINPUT_STATE
            if win.XInputGetState(nil, &controller_state) == .SUCCESS {
                //pad := &controller_state.Gamepad
            }
        }

        tm := f32(time.duration_seconds(time.tick_since(tick)))

        trans := linalg.MATRIX4F32_IDENTITY
        //trans *= linalg.matrix4_translate_f32([3]f32{300, 116, 0.0})
        //trans *= linalg.matrix4_scale([3]f32{2, 2, 0})
        trans *= proj

        mpds : dx11.MAPPED_SUBRESOURCE


        d3context->Map(constant_buffer, 0, .WRITE_DISCARD, {}, &mpds)
        {
            constants := (^Constants)(mpds.pData)
            constants.transform = trans
            constants.time = tm
        }

        d3context->Unmap(constant_buffer, 0)

        draw_text("Hello", 200, 200)
        draw_text("Nijesam_siguran.", 500, 400)

        draw_rectangle(Rect{500, 100, 200, 100}, RED)
        draw_text("vito", 566, 141)
        draw_rectangle(Rect{550, 150, 200, 100}, WHITE)
        draw_text_color("vito", 616, 191, RED)
        draw_rectangle(Rect{600, 200, 200, 100}, BLUE)
        draw_text_color("vito", 666, 241, BLACK)
        draw_rectangle(Rect{650, 250, 200, 100}, BLACK)
        draw_text_color("vito", 716, 291, RED)

        draw_texture(.Sprite_0002, Rect {1000, 100, 64, 64})

        if len(vertices) > (8 * 6) {
            if vbuffer != nil {vbuffer->Release()}
            vdesc.ByteWidth = u32(len(vertices) * size_of(f32))
            srdata.pSysMem = raw_data(vertices)
            device->CreateBuffer(&vdesc, &srdata, &vbuffer)
        }
        d3context->ClearRenderTargetView(bbuffer_target, &[4]f32{0.08, 0.58, 0.2, 1})

        stride : u32 = 8 * 4
        offset : u32 = 0

        d3context->IASetInputLayout(input_layout)
        d3context->IASetVertexBuffers(0, 1, &vbuffer, &stride, &offset)
        d3context->IASetPrimitiveTopology(.TRIANGLELIST)

        d3context->VSSetShader(vs, nil, 0)
        d3context->VSSetConstantBuffers(0, 1, &constant_buffer)
        d3context->PSSetShader(ps, nil, 0)
        d3context->PSSetConstantBuffers(0, 1, &constant_buffer)

        d3context->PSSetShaderResources(0, 1, &cmap)
        d3context->PSSetSamplers(0, 1, &cmap_sampler)

        d3context->Draw(u32(len(vertices) / 8), 0)

        shrink(&vertices, 8 * 6)

        swap_chain->Present(1, nil)
    }

    delete(vertices)
    for _, value in track.allocation_map {
        fmt.printf("%v leaked %v bytes", value.location, value.size)
    }
}

shaders_hlsl := `
cbuffer constants : register(b0) {
    float4x4 transform;
    float time;
    int idx;
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
    //output.tex0 = float2(input.tex0.x, 1.0f - input.tex0.y);
    return output;
}

float4 ps_main(vout input) : SV_TARGET {
    return colormap.Sample(color_sampler, input.tex0) * float4(input.color);
}`

vertices := make([dynamic]f32)



atlas_textures: [TextureName]Atlas_Texture = {
	.None = {},
	.Sprite_0001 = { rect = {450, 54, 16, 14}, offset_top = 0, offset_right = 0, offset_bottom = 2, offset_left = 0, document_size = {16, 16}, duration = 0.100},
	.Sprite_0002 = { rect = {34, 0, 28, 27}, offset_top = 2, offset_right = 2, offset_bottom = 3, offset_left = 2, document_size = {32, 32}, duration = 0.100},
}

atlas_glyphs: []Atlas_Glyph = {
	{ rect = {484, 28, 18, 25}, value = 'A', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {424, 28, 18, 25}, value = 'B', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {384, 28, 18, 25}, value = 'C', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {344, 28, 18, 25}, value = 'D', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {284, 28, 18, 25}, value = 'E', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {244, 28, 18, 25}, value = 'F', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {164, 28, 18, 25}, value = 'G', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {415, 1, 18, 25}, value = 'H', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {115, 29, 11, 25}, value = 'I', offset_x = 0, offset_y = 0, advance_x = 17},
	{ rect = {128, 29, 11, 25}, value = 'J', offset_x = 0, offset_y = 0, advance_x = 17},
	{ rect = {195, 1, 18, 25}, value = 'K', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {215, 1, 18, 25}, value = 'L', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {235, 1, 18, 25}, value = 'M', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {255, 1, 18, 25}, value = 'N', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {275, 1, 18, 25}, value = 'O', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {435, 1, 18, 25}, value = 'P', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {315, 1, 18, 25}, value = 'Q', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {335, 1, 18, 25}, value = 'R', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {355, 1, 18, 25}, value = 'S', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {375, 1, 18, 25}, value = 'T', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {395, 1, 18, 25}, value = 'U', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {464, 28, 18, 25}, value = 'V', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {144, 1, 25, 25}, value = 'W', offset_x = 0, offset_y = 0, advance_x = 32},
	{ rect = {455, 1, 18, 25}, value = 'X', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {475, 1, 18, 25}, value = 'Y', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {144, 28, 18, 25}, value = 'Z', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {368, 55, 18, 18}, value = 'a', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {184, 28, 18, 25}, value = 'b', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {408, 55, 18, 18}, value = 'c', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {224, 28, 18, 25}, value = 'd', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {328, 55, 18, 18}, value = 'e', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {1, 36, 11, 25}, value = 'f', offset_x = 0, offset_y = 0, advance_x = 17},
	{ rect = {64, 1, 18, 26}, value = 'g', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {304, 28, 18, 25}, value = 'h', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {27, 36, 4, 25}, value = 'i', offset_x = 0, offset_y = 0, advance_x = 10},
	{ rect = {1, 1, 12, 33}, value = 'j', offset_x = -4, offset_y = 0, advance_x = 14},
	{ rect = {364, 28, 18, 25}, value = 'k', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {504, 1, 4, 25}, value = 'l', offset_x = 0, offset_y = 0, advance_x = 10},
	{ rect = {201, 55, 25, 18}, value = 'm', offset_x = 0, offset_y = 6, advance_x = 32},
	{ rect = {388, 55, 18, 18}, value = 'n', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {348, 55, 18, 18}, value = 'o', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {124, 1, 18, 26}, value = 'p', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {104, 1, 18, 26}, value = 'q', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {428, 55, 11, 18}, value = 'r', offset_x = 0, offset_y = 6, advance_x = 17},
	{ rect = {308, 55, 18, 18}, value = 's', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {141, 55, 11, 25}, value = 't', offset_x = 0, offset_y = 0, advance_x = 17},
	{ rect = {288, 55, 18, 18}, value = 'u', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {268, 55, 18, 18}, value = 'v', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {174, 55, 25, 18}, value = 'w', offset_x = 0, offset_y = 6, advance_x = 32},
	{ rect = {248, 55, 18, 18}, value = 'x', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {84, 1, 18, 26}, value = 'y', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {228, 55, 18, 18}, value = 'z', offset_x = 0, offset_y = 6, advance_x = 24},
	{ rect = {14, 36, 11, 25}, value = '1', offset_x = 0, offset_y = 0, advance_x = 17},
	{ rect = {264, 28, 18, 25}, value = '2', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {204, 28, 18, 25}, value = '3', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {295, 1, 18, 25}, value = '4', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {444, 28, 18, 25}, value = '5', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {95, 29, 18, 25}, value = '6', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {324, 28, 18, 25}, value = '7', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {404, 28, 18, 25}, value = '8', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {35, 29, 18, 25}, value = '9', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {75, 29, 18, 25}, value = '0', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {55, 29, 18, 25}, value = '?', offset_x = 0, offset_y = 0, advance_x = 24},
	{ rect = {504, 28, 4, 25}, value = '!', offset_x = 0, offset_y = 0, advance_x = 10},
	{ rect = {171, 1, 22, 25}, value = '&', offset_x = 0, offset_y = 0, advance_x = 28},
	{ rect = {489, 55, 8, 8}, value = '.', offset_x = 0, offset_y = 16, advance_x = 14},
	{ rect = {468, 55, 8, 12}, value = ',', offset_x = 0, offset_y = 16, advance_x = 14},
	{ rect = {33, 56, 18, 5}, value = '_', offset_x = 0, offset_y = 27, advance_x = 24},
	{ rect = {441, 55, 8, 18}, value = ':', offset_x = 0, offset_y = 6, advance_x = 14},
	{ rect = {25, 1, 8, 33}, value = '[', offset_x = 0, offset_y = 0, advance_x = 14},
	{ rect = {15, 1, 8, 33}, value = ']', offset_x = 0, offset_y = 0, advance_x = 14},
	{ rect = {499, 55, 11, 5}, value = '-', offset_x = 0, offset_y = 9, advance_x = 17},
	{ rect = {154, 55, 18, 19}, value = '+', offset_x = 0, offset_y = 2, advance_x = 24},
}

