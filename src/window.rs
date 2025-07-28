use image::GenericImageView;
use imgui::*;
use imgui_wgpu::{Renderer, RendererConfig};
use imgui_winit_support::WinitPlatform;
use pollster::block_on;
use std::{fs, sync::Arc, time::Instant};
use wgpu::SamplerDescriptor;
use winit::{
    application::ApplicationHandler,
    dpi::{LogicalPosition, LogicalSize, Position},
    event::{Event, WindowEvent},
    event_loop::ActiveEventLoop,
    keyboard::{Key, NamedKey},
    window::Window,
};

use crate::{clipb, ui::chess_ui::ChessUi, util::PICE_IMAGES};

struct ImguiState {
    context: imgui::Context,
    platform: WinitPlatform,
    renderer: Renderer,
    clear_color: wgpu::Color,
    last_frame: Instant,
    last_cursor: Option<MouseCursor>,
}

struct AppWindow {
    device: wgpu::Device,
    queue: wgpu::Queue,
    window: Arc<Window>,
    surface_desc: wgpu::SurfaceConfiguration,
    surface: wgpu::Surface<'static>,
    hidpi_factor: f64,
    imgui: Option<ImguiState>,
}

pub struct DrawCtx<'a> {
    pub ui: &'a mut imgui::Ui,
    pub textures: [imgui::TextureId; 12],
}

pub struct App {
    window: Option<AppWindow>,
    chess_ui: ChessUi,
    textures: Option<[imgui::TextureId; 12]>,
}

impl AppWindow {
    fn setup_gpu(event_loop: &ActiveEventLoop) -> Self {
        let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor {
            backends: wgpu::Backends::PRIMARY,
            ..Default::default()
        });

        let window_size = LogicalSize::new(900.0, 700.0);

        let window = {
            let attributes = Window::default_attributes().with_inner_size(window_size);

            Arc::new(event_loop.create_window(attributes).unwrap())
        };

        let monitor_size = window.current_monitor().unwrap().size();
        window.set_outer_position(Position::Logical(LogicalPosition::new(
            (monitor_size.width as f64 - window_size.width) / 2.0,
            (monitor_size.height as f64 - window_size.height) / 2.0,
        )));

        let size = window.inner_size();
        let hidpi_factor = window.scale_factor();
        let surface = instance.create_surface(window.clone()).unwrap();

        let adapter = block_on(instance.request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::HighPerformance,
            compatible_surface: Some(&surface),
            force_fallback_adapter: false,
        }))
        .unwrap();

        let (device, queue) =
            block_on(adapter.request_device(&wgpu::DeviceDescriptor::default())).unwrap();

        let surface_desc = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: wgpu::TextureFormat::Bgra8UnormSrgb,
            width: size.width,
            height: size.height,
            present_mode: wgpu::PresentMode::Fifo,
            desired_maximum_frame_latency: 2,
            alpha_mode: wgpu::CompositeAlphaMode::Auto,
            view_formats: vec![wgpu::TextureFormat::Bgra8Unorm],
        };

        surface.configure(&device, &surface_desc);

        let imgui = None;
        Self {
            device,
            queue,
            window,
            surface_desc,
            surface,
            hidpi_factor,
            imgui,
        }
    }

    fn setup_textures(&mut self) -> Option<[imgui::TextureId; 12]> {
        let texture_ids = PICE_IMAGES
            .iter()
            .map(|&path| {
                let dynamic_image = image::load_from_memory(
                    fs::read(path)
                        .expect("Failed to read image file")
                        .as_slice(),
                )
                .unwrap();

                let image_rgba = dynamic_image.to_rgba8();
                let image_dimensions = dynamic_image.dimensions();

                let texture_size = wgpu::Extent3d {
                    width: image_dimensions.0,
                    height: image_dimensions.1,
                    depth_or_array_layers: 1,
                };

                let renderer = &mut self.imgui.as_mut().unwrap().renderer;

                let tx = imgui_wgpu::Texture::new(
                    &self.device,
                    renderer,
                    imgui_wgpu::TextureConfig {
                        sampler_desc: SamplerDescriptor {
                            label: Some("imgui-wgpu sampler"),
                            address_mode_u: wgpu::AddressMode::ClampToEdge,
                            address_mode_v: wgpu::AddressMode::ClampToEdge,
                            address_mode_w: wgpu::AddressMode::ClampToEdge,
                            mag_filter: wgpu::FilterMode::Linear,
                            min_filter: wgpu::FilterMode::Linear,
                            mipmap_filter: wgpu::FilterMode::Nearest,
                            lod_min_clamp: 0.0,
                            lod_max_clamp: 100.0,
                            compare: None,
                            anisotropy_clamp: 1,
                            border_color: None,
                        },
                        size: texture_size,
                        mip_level_count: 1,
                        sample_count: 1,
                        dimension: wgpu::TextureDimension::D2,
                        format: Some(wgpu::TextureFormat::Rgba8UnormSrgb),
                        usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
                        label: Some(&format!("texture {}", path)),
                    },
                );

                self.queue.write_texture(
                    wgpu::TexelCopyTextureInfo {
                        texture: &tx.texture(),
                        mip_level: 0,
                        origin: wgpu::Origin3d::ZERO,
                        aspect: wgpu::TextureAspect::All,
                    },
                    &image_rgba,
                    wgpu::TexelCopyBufferLayout {
                        offset: 0,
                        bytes_per_row: Some(4 * image_dimensions.0),
                        rows_per_image: Some(image_dimensions.1),
                    },
                    texture_size,
                );

                renderer.textures.insert(tx)
            })
            .collect::<Vec<TextureId>>();

        Some(
            texture_ids
                .as_slice()
                .try_into()
                .unwrap_or_else(|_| panic!("Expected 12 textures, got {}", texture_ids.len())),
        )
    }

    fn setup_imgui(&mut self) {
        let mut context = imgui::Context::create();
        let mut platform = imgui_winit_support::WinitPlatform::new(&mut context);

        platform.attach_window(
            context.io_mut(),
            &self.window,
            imgui_winit_support::HiDpiMode::Default,
        );
        context.set_ini_filename(None);

        let font_size = (13.0 * self.hidpi_factor) as f32;
        context.io_mut().font_global_scale = (1.0 / self.hidpi_factor) as f32;

        context.fonts().add_font(&[FontSource::DefaultFontData {
            config: Some(imgui::FontConfig {
                oversample_h: 1,
                pixel_snap_h: true,
                size_pixels: font_size,
                ..Default::default()
            }),
        }]);

        let clipboard = clipb::Clipboard::new();
        context.set_clipboard_backend(clipboard);

        let clear_color = wgpu::Color {
            r: 0.1,
            g: 0.2,
            b: 0.3,
            a: 1.0,
        };

        let renderer_config = RendererConfig {
            texture_format: self.surface_desc.format,
            ..Default::default()
        };

        let renderer = Renderer::new(&mut context, &self.device, &self.queue, renderer_config);
        let last_frame = Instant::now();
        let last_cursor = None;

        self.imgui = Some(ImguiState {
            context,
            platform,
            renderer,
            clear_color,
            last_frame,
            last_cursor,
        })
    }

    fn new(event_loop: &ActiveEventLoop) -> Self {
        let mut window = Self::setup_gpu(event_loop);
        window.setup_imgui();
        window
    }
}

impl App {
    pub fn new(chess_ui: ChessUi) -> Self {
        App {
            window: None,
            textures: None,
            chess_ui,
        }
    }
}

impl ApplicationHandler for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        let mut window = AppWindow::new(event_loop);
        self.textures = window.setup_textures();
        self.window = Some(window);
    }

    fn window_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        window_id: winit::window::WindowId,
        event: WindowEvent,
    ) {
        let window = self.window.as_mut().unwrap();
        let imgui = window.imgui.as_mut().unwrap();

        match &event {
            WindowEvent::Resized(size) => {
                window.surface_desc = wgpu::SurfaceConfiguration {
                    usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
                    format: wgpu::TextureFormat::Bgra8UnormSrgb,
                    width: size.width.max(1),
                    height: size.height.max(1),
                    present_mode: wgpu::PresentMode::Fifo,
                    desired_maximum_frame_latency: 2,
                    alpha_mode: wgpu::CompositeAlphaMode::Auto,
                    view_formats: vec![wgpu::TextureFormat::Bgra8Unorm],
                };

                window
                    .surface
                    .configure(&window.device, &window.surface_desc);
            }
            WindowEvent::CloseRequested => event_loop.exit(),
            WindowEvent::KeyboardInput { event, .. } => {
                if let Key::Named(NamedKey::Escape) = event.logical_key {
                    if event.state.is_pressed() {
                        event_loop.exit();
                    }
                }
            }
            WindowEvent::RedrawRequested => {
                let delta_s = imgui.last_frame.elapsed();
                let now = Instant::now();
                imgui
                    .context
                    .io_mut()
                    .update_delta_time(now - imgui.last_frame);
                imgui.last_frame = now;

                let frame = match window.surface.get_current_texture() {
                    Ok(frame) => frame,
                    Err(e) => {
                        eprintln!("dropped frame: {e:?}");
                        return;
                    }
                };

                imgui
                    .platform
                    .prepare_frame(imgui.context.io_mut(), &window.window)
                    .expect("Failed to prepare frame");
                let ui = imgui.context.frame();

                {
                    if let Some(textures) = self.textures {
                        self.chess_ui.draw(DrawCtx { ui, textures });
                    }

                    let fps = 1.0 / delta_s.as_secs_f64();
                    window.window.set_title(&format!(
                        "chess | fps={:.1}, ft={:.3} ms",
                        fps,
                        delta_s.as_secs_f64() * 1000.0
                    ));
                }

                let mut encoder: wgpu::CommandEncoder = window
                    .device
                    .create_command_encoder(&wgpu::CommandEncoderDescriptor { label: None });

                if imgui.last_cursor != ui.mouse_cursor() {
                    imgui.last_cursor = ui.mouse_cursor();
                    imgui.platform.prepare_render(ui, &window.window);
                }

                let view = frame
                    .texture
                    .create_view(&wgpu::TextureViewDescriptor::default());

                let mut rpass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                    label: None,
                    color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                        view: &view,
                        resolve_target: None,
                        ops: wgpu::Operations {
                            load: wgpu::LoadOp::Clear(imgui.clear_color),
                            store: wgpu::StoreOp::Store,
                        },
                    })],
                    depth_stencil_attachment: None,
                    timestamp_writes: None,
                    occlusion_query_set: None,
                });

                imgui
                    .renderer
                    .render(
                        imgui.context.render(),
                        &window.queue,
                        &window.device,
                        &mut rpass,
                    )
                    .expect("Rendering failed");

                drop(rpass);

                window.queue.submit(Some(encoder.finish()));

                frame.present();
            }
            _ => (),
        }

        imgui.platform.handle_event::<()>(
            imgui.context.io_mut(),
            &window.window,
            &Event::WindowEvent { window_id, event },
        );
    }

    fn user_event(&mut self, _event_loop: &ActiveEventLoop, event: ()) {
        let window = self.window.as_mut().unwrap();
        let imgui = window.imgui.as_mut().unwrap();

        imgui.platform.handle_event::<()>(
            imgui.context.io_mut(),
            &window.window,
            &Event::UserEvent(event),
        );
    }

    fn device_event(
        &mut self,
        _event_loop: &ActiveEventLoop,
        device_id: winit::event::DeviceId,
        event: winit::event::DeviceEvent,
    ) {
        let window = self.window.as_mut().unwrap();
        let imgui = window.imgui.as_mut().unwrap();

        imgui.platform.handle_event::<()>(
            imgui.context.io_mut(),
            &window.window,
            &Event::DeviceEvent { device_id, event },
        );
    }

    fn about_to_wait(&mut self, _event_loop: &ActiveEventLoop) {
        let window = self.window.as_mut().unwrap();
        let imgui = window.imgui.as_mut().unwrap();

        window.window.request_redraw();
        imgui.platform.handle_event::<()>(
            imgui.context.io_mut(),
            &window.window,
            &Event::AboutToWait,
        );
    }
}
