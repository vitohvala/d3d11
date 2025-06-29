package win32_project0

import "core:fmt"
import win "core:sys/windows"
import "core:slice"
import "core:mem"
import "base:runtime"
import "core:log"
import "core:time"
import dx11 "vendor:directx/d3d11"
import dxgi "vendor:directx/dxgi"
import d3d  "vendor:directx/d3d_compiler"
import stbi "vendor:stb/image"

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


main :: proc() {
    context.logger = log.create_console_logger()

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

    vertices := [?]f32{
    //   position        texc   color
         0.75,  0.75, 0, 1, 1,  1, 0, 0,
         0.75, -0.75, 0, 1, 0,  0, 1, 0,
        -0.75, -0.75, 0, 0, 0,  0, 0, 1,
        -0.75, -0.75, 0, 0, 0,  0, 0, 1,
        -0.75,  0.75, 0, 0, 1,  0, 1, 0,
         0.75,  0.75, 0, 1, 1,  1, 0, 0,
    }

    vdesc : dx11.BUFFER_DESC

    vdesc.Usage = .DEFAULT
    vdesc.BindFlags = {.VERTEX_BUFFER}
    vdesc.ByteWidth = size_of(vertices)

    srdata : dx11.SUBRESOURCE_DATA

    srdata.pSysMem = &vertices

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
    image_data := stbi.load("container.jpg", &twidth, &theight, &nr_channels, 4)
    assert(image_data != nil)

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

        mpds : dx11.MAPPED_SUBRESOURCE

        tm := f32(time.duration_seconds(time.tick_since(tick)))

        d3context->Map(constant_buffer, 0, .WRITE_DISCARD, {}, &mpds)
        {
            constants := (^Constants)(mpds.pData)
            constants.time = tm
        }

        d3context->Unmap(constant_buffer, 0);

        d3context->ClearRenderTargetView(bbuffer_target, &[4]f32{1, 1, 1, 1})

        stride : u32 = 8 * 4
        offset : u32 = 0

        d3context->IASetInputLayout(input_layout)
        d3context->IASetVertexBuffers(0, 1, &vbuffer, &stride, &offset)
        d3context->IASetPrimitiveTopology(.TRIANGLELIST)

        d3context->VSSetShader(vs, nil, 0)
        //d3context->VSSetConstantBuffers(0, 1, &constant_buffer)
        d3context->PSSetShader(ps, nil, 0)
        d3context->PSSetConstantBuffers(0, 1, &constant_buffer)

        d3context->PSSetShaderResources(0, 1, &cmap)
        d3context->PSSetSamplers(0, 1, &cmap_sampler)

        d3context->Draw(6, 0)

        swap_chain->Present(1, nil)
    }
}

shaders_hlsl := `
cbuffer constants : register(b0) {
    float time;
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
    output.position = input.position;
    output.color = float4(input.color, 1.0f);
    output.tex0 = input.tex0;
    return output;
}
float4 ps_main(vout input) : SV_TARGET {
    //float2 pos1 = input.position.xy;
    // pos1.x += (time * 32);
    //float2 res = float2(1280.0f, 720.0f);
    //float2 pos = (pos1 - res) / res.y;
    //float ds = smoothstep(2.0, 0.0, length(pos));
    //float3 a = float3(0.15, 0.2, 0.25);
    //float3 b = float3(0.4, 0.3, 0.4);
    //float3 col = a + b * cos(6 * (time * ds));
    //return colormap.Sample(color_sampler, input.tex0) * float4(col, 1.0f);
    //float PatternMask = fmod(pos.x + pos.y, 2.0);
    //return PatternMask * float4(1, 1, 1, 1);
    return colormap.Sample(color_sampler, input.tex0) * input.color;
}`